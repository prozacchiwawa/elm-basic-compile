var q = require('q');
var XMLHttpRequest = XMLHttpRequest || require('xmlhttprequest').XMLHttpRequest;
var semver = require("semver");
var ls = require("logic-solver");
var btoa = btoa || require('btoa');
var pv = require("./package-version");
var elmStatic = require("./elm-static");
var compiler = require("./elm-compiler-interface");
if (typeof localStorage === "undefined" || localStorage === null) {
  var LocalStorage = require('node-localstorage').LocalStorage;
  localStorage = new LocalStorage('./elm-cache');
}

var defaultImports = [
    ["Basics"],
    ["Debug"],
    ["List"],
    ["Maybe"],
    ["Result"],
    ["String"],
    ["Tuple"],
    ["Platform"],
    ["Platform","Cmd"],
    ["Platform","Sub"]
];

/* 
 * user,project: github.com/<user>/<project>
 * var projectSpec =
 *  { user: "prozacchiwawa",
 *    project: "effmodel",
 *    version: "2.0.1"
 *  };
 * retriever.retrieveJson(projectSpec) ->
 * In one implementation, a promise to retrieve
 * "https://raw.githubusercontent.com/prozacchiwawa/effmodel/2.0.1/elm-package.json"
 * retriever.retrieveSource(projectSpec,"src",["EffModel"],"elm") -> 
 * In one implementation, a promise to retrieve
 * "https://raw.githubusercontent.com/prozacchiwawa/effmodel/2.0.1/src/EffModel.elm"
 * Could be a promise for local storage cache, file read etc.
 */
function ElmPackage(retriever,projectSpec,solver) {
    this.solver = solver || new PackageSolver(retriever);
    this.projectSpec = projectSpec;
    this.retriever = retriever;
    this.elmPackage = null;
    this.exactDeps = null;
    this.packageDeps = {};
    this.importsRe = /^\/\/[ ]*import[ ]+([^\/\n\r]*)\/\//m;
    this.wsRe = /^\s+|\s+$/g;
    var self = this;
    this.solver.have = this.solver.have || {};
    var pname = pv.packageNameString(projectSpec);
    this.solver.have[pname] = this;
    this.solver.deps = {};
    this.compiler = null;
    this.afterElmPackage = compiler.init().then(function(comp) {
        self.compiler = comp;
    }).then(function() {
        return retriever.retrieveJson(projectSpec);
    }).then(function(pspec) {
        return JSON.parse(pspec);
    }).then(function(pspec) {
        self.solver.injectPackage(projectSpec,pspec);
        return self.solver.fillTree(projectSpec).then(function() { return pspec; });
    }).then(function(pspec) {
        var solution = self.solver.solve(projectSpec);
        self.exactDeps = solution;
        return pspec;
    }).then(function(pspec) {
        self.elmPackage = pspec;
        return self.elmPackage;
    });
}

/* 
 * return a promise for a pair of elm source file or null, 
 * javascript source file or null
 * 
 * If neither exists, reject with a message.
 * If either gives an error other than 404, reject with a message.
 */
ElmPackage.prototype.findSourceFiles = function(modname) {
    function nullOn404(err) {
        if (err.status == 404) {
            return null;
        } else {
            throw err;
        }
    }
    var self = this;
    function tryOneSourceDir(sourceDir,modname,ext) {
        return self.retriever.retrieveSource(
            self.projectSpec,
            sourceDir,
            modname,
            ext).
            fail(nullOn404);
    }
    function tryAllSourceDirs(results,sourceDirs,modname) {
        for (var i = 0; i < sourceDirs.length; i++) {
            var sourceDir = sourceDirs[i];
            if (modname[0] == "Native") {
                results.push(tryOneSourceDir(sourceDir,modname,"js").
                             then(function(file) { return ["js",file]; }));
            } else {
                results.push(tryOneSourceDir(sourceDir,modname,"elm").
                             then(function(file) { return ["elm",file]; }));
            }
        }
    }
    var promise = null;
    function createPromiseForFiles(modname) {
        var results = [];
        tryAllSourceDirs(
            results,
            self.elmPackage["source-directories"],
            modname);
        return q.all(results);
    }

    if (this.elmPackage == null) {
        this.afterElmPackage = this.afterElmPackage.then(function() {
            return createPromiseForFiles(modname);
        });
        return this.afterElmPackage;
    } else {
        return createPromiseForFiles(modname);
    }
}

ElmPackage.prototype._collectPkgDeps = function(downstreamPackages) {
    var name = pv.packageNameString(self.productName);
    downstreamPackages.push(name);
    for (var k in self.packageDeps) {
        self.packageDeps[k]._collectPkgDeps(downstreamPackages);
    }    
}

ElmPackage.prototype.expandPackage = function(extra) {
    var self = this;
    if (this.elmPackage == null) {
        this.afterElmPackage = this.afterElmPackage.then(function() {
            return self.expandPackage(extra);
        });
        return this.afterElmPackage;
    }
    var packageName = pv.packageNameString(this.projectSpec);
    var exposed = this.elmPackage["exposed-modules"].map(function(m) { return m; });
    if (extra) {
        exposed.push.apply(exposed,extra);
    }

    console.log("/* expandPackage",self.projectSpec,exposed,"*/");
    // Put in exposed modules from imports
    self.solver.have = self.solver.have || {};
    var downstreamPackages = Object.keys(self.elmPackage.dependencies);
    return self._expandDep(downstreamPackages,0).then(function() {
        downstreamPackages.map(function(k) {
            var ver = self.exactDeps[k];
            var specAr = k.split("/");
            var modspec = {user: specAr[0], project: specAr[1], version: ver};
            self.packageDeps[k] = self.solver.have[k];
        });
    });
}

ElmPackage.prototype._findPackageForModule = function(m) {
    var self = this;
    for (var p in self.solver.have) {
        var pkg = self.solver.have[p];
        if (pkg.elmPackage["exposed-modules"].indexOf(m) != -1) {
            return pkg;
        }
    }
    return self;
}

ElmPackage.prototype._compileIfNeeded = function(mods,i) {
    var self = this;
    if (i >= mods.length) {
        console.log("/* Done compiling */");
        return q.fcall(function() { });
    }
    var m = mods[i];
    if (typeof(m) !== 'string') {
        m = m.join('.');
    }
    var who = self._findPackageForModule(m);
    var promise = null;

    console.log("/* _compileIfNeeded",who.projectSpec,m,"*/");

    who.solver.deps[m] = who.solver.deps[m] || {name: m, elm: null, js: null, imports: null, pkg: who};
    var s = who.solver.deps[m];

    console.log("/* _compileIfNeeded */");

    if (!s.elm && !s.js) {
        console.log("/* Get source for",m,"*/");
        return who.findSourceFiles(m.split(".")).then(function(sres) {
            console.log("/* Got source for",m,"*/");
            var res = sres[0];
            s[res[0]] = res[1];
            if (s.js) {
                var jsImport = s.js.match(self.importsRe);
                if (jsImport != null) {
                    var importsStrip = jsImport[1].replace(self.wsRe, '');
                    s.imports = importsStrip.split(',').map(function(i) {
                        return i.replace(self.wsRe, '');
                    }).filter(function(imp) {
                        return imp !== '';
                    });
                }
            } else if (s.elm) {
                var intf = localStorage.getItem(m + ":interface");
                var elmo = localStorage.getItem(m + ":elmo");
                s.intf = intf;
                s.elmo = elmo;
            }
            console.log("/* Src */");
        }).fail(function(e) {
            console.error(e);
            throw e;
        }).then(function() {
            console.log("/* New compile cycle */");
            return self._compileIfNeeded(mods,i);
        });
    } else if (!s.imports) {
        console.log("/* Parse",s.name,"*/");
        return who.compiler.parse([who.projectSpec.user,who.projectSpec.project], s.elm).then(function(res) {
            console.log("/* Got Imports",s.pkg.projectSpec,m,res,"*/");
            s.imports = res[1].map(function(mod) {
                return mod.join(".");
            });
            return who._compileIfNeeded(s.imports,0);
        }).then(function() { return self._compileIfNeeded(mods,i); });
    } else if (s.elm && !s.intf) {
        console.log("/* Interface",m,"*/");
        var who = self._findPackageForModule(m);
        var needModules = s.imports.filter(function(k) {
            return !self.solver.deps[k] && k.split('.')[0] !== 'Native';
        });
        if (needModules.length > 0) {
            console.log("/* Unretrieved mods",needModules,"*/");
            return who._compileIfNeeded(needModules,0).then(function() {
                console.log("/* Continue compilation */");
                return self._compileIfNeeded(mods,i);
            });
        }
        var compileInputs = [];
        var ifaces = Object.keys(self.solver.deps).filter(function(k) {
            return k.split('.')[0] !== 'Native' &&
                self.solver.deps[k].intf;
        });
        for (var j = 0; j < ifaces.length; j++) {
            var k = ifaces[j];
            var ss = self.solver.deps[k];
            var whoj = ss.pkg;
            var spkg = [whoj.projectSpec.user,whoj.projectSpec.project];
            console.log("/* Import",k,"in",spkg,"*/");
            compileInputs.push([
                [[spkg,k.split(".")],whoj.projectSpec.version],
                ss.intf
            ]);
        }
        var name = [who.projectSpec.user, who.projectSpec.project];
        var exposed = who.elmPackage["exposed-modules"];
        var isExposed = exposed.indexOf(m) != -1 ? "true" : "false";
        var source = self.solver.deps[m].elm;
        console.log("/* Compile",m,isExposed,JSON.stringify(compileInputs.map(function(i) { return i[0]; })),"*/");
        return self.compiler.compile(name,isExposed,source,compileInputs).fail(function(error) {
            console.error("compiling",name,error);
            throw error;
        }).then(function(res) {
            if (res[0] !== "false") {
                localStorage.setItem(m + ":interface", res[1]);
                localStorage.setItem(m + ":elmo", res[2]);
                self.solver.deps[m].intf = res[1];
                self.solver.deps[m].elmo = res[2];
            } else {
                throw new Error(res[1].join("\n"));
            }
            return [res[0] !== "false", res[1], res[2]];
        }).then(function() { return self._compileIfNeeded(mods,i); });
    } else {
        console.log("/* Mod advance",i,mods,"*/");
        return self._compileIfNeeded(mods,i+1);
    }
}

ElmPackage.prototype.compileModule = function(modname) {
    var self = this;
    var pkg = modname.split("/");
    if (this.elmPackage == null) {
        this.afterElmPackage = this.afterElmPackage.then(function() {
            return self.compileModule(modname);
        });
        return this.afterElmPackage;
    }

    var who = self._findPackageForModule(modname);
    return who._compileIfNeeded([modname],0);
}

ElmPackage.prototype._collect = function(compileOrder,m) {
    var self = this;
    if (compileOrder[m]) {
        return;
    }
    var who = self._findPackageForModule(m);
    compileOrder[m] = compileOrder[m] || {};
    var ideps = self.solver.deps[m];
    var imports = ideps.imports || [];
    for (var i = 0; i < imports.length; i++) {
        var k = imports[i];
        compileOrder[m][k] = true;
        who._collect(compileOrder,k);
    }
}

ElmPackage.prototype._linkOne = function(compileOrder,result) {
    var self = this;
    for (var k in compileOrder) {
        var o = Object.keys(compileOrder[k]).filter(function(m) {
            return !!compileOrder[m];
        });

        console.log("/* remaining to link for",k,":",o,"*/");

        if (o.length != 0) {
            continue;
        }

        var m = k;
        var modname = m.split(".");
        var js = null;
        var who = self._findPackageForModule(m);
        if (modname[0] == "Native") {
            if (!who.solver.deps[m].js) {
                throw new Error("Could not find js source for"+who.projectSpec+":"+m);
            }
            js = who.solver.deps[m].js;
        } else {
            if (!who.solver.deps[m].elmo) {
                console.log("/* Make ELMO",m,"*/");
                return who.compileModule(m).then(function() {
                    return self._linkOne(compileOrder,result);
                });
            }
            js = who.solver.deps[m].elmo;
        }
        result.push("//***"+modname+" "+JSON.stringify(who.projectSpec));
        result.push(js);
        delete compileOrder[m];
        return self._linkOne(compileOrder,result);
    }
    return q.fcall(function() { return result; });
}

ElmPackage.prototype._expandDep = function(mods,i) {
    var self = this;
    if (i >= mods.length) {
        return q.fcall(function() { });
    }
    var m = mods[i];
    console.log("/* _expandDep",m,"*/");
    if (!self.solver.have[m]) {
        var ver = self.exactDeps[m];
        var specAr = m.split("/");
        var modspec = {user: specAr[0], project: specAr[1], version: ver};
        var pkg = new ElmPackage(self.retriever,modspec,self.solver);
        self.solver.have[m] = pkg;
    }
    return self.solver.have[m].expandPackage().then(function() {
        return self._expandDep(mods,i+1);
    });
}

ElmPackage.prototype.link = function(mods) {
    var self = this;
    if (this.elmPackage == null) {
        this.afterElmPackage = this.afterElmPackage.then(function() {
            return self.link(mods);
        });
        return this.afterElmPackage;
    }

    var result = [];
    var compileOrderKeys = Object.keys(self.solver.deps);
    if (mods) {
        compileOrderKeys.push.apply(compileOrderKeys,mods);
    }
    
    var compileOrder = {};
    for (var i = 0; i < compileOrderKeys.length; i++) {
        var k = compileOrderKeys[i];
        self._collect(compileOrder, k);
    }
    
    console.log("/*Link:",compileOrder,"*/");
    return self._expandDep(Object.keys(self.packageDeps),0).then(function() {
        return self._linkOne(compileOrder, result);
    }).then(function(result) {
        var fin = [elmStatic.prelude.join("\n")];
        fin.push.apply(fin,result);
        fin.push(elmStatic.footer.join("\n"));
        fin = fin.join("\n");
        return fin;
    });
}

/* 
 * A solver for package versions using semver.
 * It contains a map of package names to version lists, and another
 * map of package names to version constraint lists.
 * Each time the user adds a package, any packages that have unmet
 * constraints are removed.
 *
 * solver.getBestDeps() will retrieve a list of packageSpec that meets
 * the criteria or null.
 *
 * retriever is an object that can fetch new package version lists and
 * elm-package.json.
 */
function PackageSolver(retriever) {
    this.retriever = retriever;
    this.solver = new ls.Solver();
    this.buildSolution = null;
    this.versions = {};
}

PackageSolver.prototype._makeDepsPromise = function(pname) {
    var self = this;
    return function(ver) {
        self._promiseDeps(self.versions[pname][ver]);
    }
}

/*
 * Manually create a package, given a packageSpec and an elm-package json.
 */
PackageSolver.prototype.injectPackage = function(packageSpec, json) {
    var pname = pv.packageNameString(packageSpec);
    this.versions[pname] = this.versions[pname] || {};
    this.versions[pname][json.version] = json;
}

/*
 * Promise a filled dep tree for a specific package.
 */
PackageSolver.prototype.fillTree = function(packageSpec) {
    var simpleName = pv.packageNameString(packageSpec);
    var vset = Object.keys(this.versions[simpleName]);
    var self = this;
    return q.all(vset.map(function(v) {
        return self._promiseDeps(self.versions[simpleName][v]);
    }));
}

/* Promise all dependencies to be retrieved. */
PackageSolver.prototype._promiseDeps = function(json) {
    var self = this;
    var depkeys = Object.keys(json.dependencies);
    var wantdeps = depkeys.filter(function(k) { return !self.versions[k]; });
    return q.all(wantdeps.map(function(k) {
        var reqspec = pv.parsePackageName(k);
        return self.retriever.retrieveTags(reqspec).then(function(tags) {
            return q.all(tags.map(function(v) {
                var pspec = pv.parsePackageName(k);
                pspec.version = v;
                return self.retriever.retrieveJson(pspec).then(function(j) {
                    var json = JSON.parse(j);
                    self.versions[k] = self.versions[k] || {};
                    self.versions[k][v] = json;
                    return json;
                });
            }));
        }).then(function() {
            var pspec = pv.parsePackageName(k);
            return self.fillTree(pspec);
        });
    }));
}

PackageSolver.prototype._runPackage = function(usedPackages,pkg) {
    var solver = this.solver;
    var captured = this.versions;
    var packageName = pv.packageNameString(pkg);
    var versions = Object.keys(captured[packageName]);
    for (var j = 0; j < versions.length; j++) {
        var v = versions[j];
        var fullPackageName = packageName + ":" + v;
        if (usedPackages[fullPackageName]) {
            continue;
        }
        usedPackages[fullPackageName] = true;
        var json = captured[packageName][v];
        var deps = json.dependencies;
        var depkeys = Object.keys(deps);
        for (var l = 0; l < depkeys.length; l++) {
            var dep = depkeys[l];
            var depexp = deps[dep];
            var allowedVersions = pv.getAllowedVersionsOfPackage(captured, dep, depexp);
            var disallowedVersions = pv.getNotAllowedVersionsOfPackage(captured, dep, allowedVersions);
            var diskeys = disallowedVersions.map(function(v) {
                return dep + ":" + v;
            });
            for (var i = 0; i < diskeys.length; i++) {
                solver.require(ls.implies(diskeys[i], ls.not(fullPackageName)));
            }
            var verkeys = allowedVersions.map(function(v) {
                return dep + ":" + v;
            });
            solver.require(ls.implies(fullPackageName, ls.or.apply(ls, verkeys)));
            for (var m = 0; m < allowedVersions.length; m++) {
                var up = dep.split("/");
                var allow = allowedVersions[m];
                var usePspec = { user: up[0], project: up[1], version: allow };
                this._runPackage(usedPackages,usePspec);
            }
        }
    }
}

PackageSolver.prototype.solve = function(pspec) {
    var expr = {_:""};
    var captured = this.versions;
    var packageName = pv.packageNameString(pspec) + ":" + pspec.version;
    this.solver.require(packageName);
    this._runPackage(expr, pspec);
    var solution = this.solver.solve();
    if (!solution) {
        this.buildSolution = null;
        return null;
    }
    var deps = solution.getTrueVars();
    var res = {};
    for (var i = 0; i < deps.length; i++) {
        var s = deps[i].split(':');
        res[s[0]] = s[1];
    }
    this.buildSolution = res;
    return res;
}

/*
 * The haskell code can tell us what elmi files will be required to compile
 * a specific module given a set of exact dependencies, which have been
 * created by PackageSolver::solve.  We must however build to elmi.
 *
 * Once elmo files are generated, we link by concatenating javascript from
 * the data store.
 */

module.exports.ElmPackage = ElmPackage;
module.exports.PackageSolver = PackageSolver;
