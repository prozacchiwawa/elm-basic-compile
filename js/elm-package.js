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
    this.internalDeps = null;
    this.elmPackage = null;
    this.exactDeps = null;
    this.packageDeps = {};
    this.importsRe = /^\/\/[ ]*import[ ]+([^\/\n\r]*)\/\//m;
    this.wsRe = /^\s+|\s+$/g;
    var self = this;
    this.solver.have = this.solver.have || {};
    var pname = pv.packageNameString(projectSpec);
    this.solver.have[pname] = this;
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

ElmPackage.prototype._expandPackageFullyStep = function(reachableSet,exposed) {
    var self = this;
    return q.all(exposed.filter(function(mod) {
        return !self.isExternalModule(mod);
    }).map(function(mod) {
        var modname = mod.split('.');
        return self.findSourceFiles(mod.split(".")).then(function(sf) {
            var s = {name: mod};
            for (var i = 0; i < sf.length; i++) {
                var ty = sf[i][0];
                var sd = sf[i][1];
                if (sd) {
                    s[ty] = sd;
                }
            }
            return s;
        });
    })).then(function(sources) {
        return q.all(sources.map(function(s) {
            if (reachableSet[s.name]) {
                return s;
            }
            reachableSet[s.name] = s;
            if (s.js) {
                var jsImport = s.js.match(self.importsRe);
                if (jsImport != null) {
                    var importsStrip = jsImport[1].replace(self.wsRe, '');
                    s.imports = importsStrip.split(',').map(function(i) {
                        return i.replace(self.wsRe, '');
                    }).filter(function(imp) {
                        return imp !== '';
                    }).filter(function(imp) {
                        return !reachableSet[imp];
                    });
                    return self._expandPackageFullyStep(reachableSet,s.imports);
                }
            } else if (s.elm) {
                return self.compiler.parse([self.projectSpec.user,self.projectSpec.project], s.elm).then(function(res) {
                    s.imports = res[1].map(function(mod) {
                        return mod.join(".");
                    });
                    return self._expandPackageFullyStep(reachableSet,s.imports);
                });
            } else {
                s.imports = [];
                self.internalDeps = reachableSet;
                return reachableSet;
            }
        })).then(function() {
            self.internalDeps = reachableSet;
            return reachableSet;
        });
    });
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
    var reachableSet = {};
    // Put in exposed modules from imports
    self.solver.have = self.solver.have || {};
    return q.all(Object.keys(self.exactDeps).filter(function(m) {
        return m != packageName && !self.packageDeps[m];
    }).map(function(k) {
        var ver = self.exactDeps[k];
        var exposed = self.solver.versions[k][ver]["exposed-modules"];
        var specAr = k.split("/");
        var modspec = {user: specAr[0], project: specAr[1], version: ver};
        if (self.solver.have[k]) {
            self.packageDeps[k] = self.solver.have[k];
        } else {
            var pkg = new ElmPackage(self.retriever,modspec,self.solver);
            self.packageDeps[k] = pkg;
            return pkg.expandPackage().then(function(r) {
                return pkg.compile().then(function() {
                    return r;
                });
            });
        }
    })).then(function() {
        return self._expandPackageFullyStep(reachableSet,exposed);
    });
}

ElmPackage.prototype.addInterfacesFrom = function(modname,ifacesObj) {
    var self = this;
    var ext = self.isExternalModule(modname);
    if (ext) {
        return self.packageDeps[ext].addInterfacesFrom(modname,ifacesObj);
    }
    self.internalDeps[modname].imports.filter(function(m) {
        return m.split(".")[0] != "Native";
    }).map(function(m) {
        ifacesObj[m] =
            localStorage.getItem(m + ":interface") ||
            self.internalDeps[m].intf;
        self.addInterfacesFrom(m,ifacesObj);
    });
}

ElmPackage.prototype.compileModule = function(modname) {
    var self = this;
    var mod = modname.split("/");
    if (this.elmPackage == null) {
        this.afterElmPackage = this.afterElmPackage.then(function() {
            return self.compileModule(modname);
        });
        return this.afterElmPackage;
    }

    if (self.isExternalModule(modname)) {
        if (self.packageDeps[modname]) {
            // We already built it
            return self.packageDeps[modname].compileModule(modname);
        }
        throw new Error("Could not find package dep " + modname);
    }

    if (self.internalDeps[modname] && self.internalDeps[modname].intf) {
        return q.fcall(function() {
            return [
                self.internalDeps[modname].intf,
                self.internalDeps[modname].elmo
            ];
        });
    }

    var lsIntf = localStorage.getItem(modname + ":interface");
    var lsElmo = localStorage.getItem(modname + ":elmo");
    if (lsIntf) {
        self.internalDeps[modname].intf = lsIntf;
        self.internalDeps[modname].elmo = lsElmo;
        return q.fcall(function() { return [true, lsIntf, lsElmo]; });
    }
    var name = [self.projectSpec.user, self.projectSpec.project];
    var exposed = self.elmPackage["exposed-modules"];
    var isExposed = exposed.indexOf(modname) != -1;
    var source = self.internalDeps[modname].elm;
    var ifacesObj = {};
    self.addInterfacesFrom(modname,ifacesObj);
    var ifaces = Object.keys(ifacesObj).map(function(m) {
        var pkg = self.isExternalModule(m);
        if (pkg) {
            pkg = pkg.split("/");
        } else {
            pkg = name;
        }
        return [
            [[[pkg[0],pkg[1]],m.split(".")],self.projectSpec.version],
            ifacesObj[m]
        ];
    });
    return self.compiler.compile(name,isExposed,source,ifaces).then(function(res) {
        if (res[0] != "false") {
            localStorage.setItem(modname + ":interface", res[1]);
            localStorage.setItem(modname + ":elmo", res[2]);
        } else {
            throw new Error(res[1].join("\n"));
        }
        return res;
    });
}

ElmPackage.prototype._compileOneMore = function(order) {
    var self = this;
    for (var k in order) {
        var imports = order[k];
        if (Object.keys(imports).filter(function(x) {
            return !!order[x];
        }).length == 0) {
            // Build a single module with 0 remaining imports.
            return self.compileModule(k).then(function(res) {
                delete order[k];
                return self._compileOneMore(order);
            });
        }
    }
}

ElmPackage.prototype.isExternalModule = function(k) {
    var self = this;
    for (var ep in self.packageDeps) {
        var ver = self.exactDeps[ep];
        var exposed = self.solver.versions[ep][ver]["exposed-modules"];
        if (exposed.indexOf(k) != -1) {
            return ep;
        }
    }
    if (self.internalDeps && !self.internalDeps[k]) {
        for (var ep in self.packageDeps) {
            var who = self.packageDeps[ep];
            if (who.internalDeps && who.internalDeps[k]) {
                return ep;
            }
        }
    }
    return null;
}

/*
 * Fully compile and cache a package.
 */
ElmPackage.prototype.compile = function() {
    var self = this;
    if (this.elmPackage == null) {
        this.afterElmPackage = this.afterElmPackage.then(function() {
            return self.compile();
        });
        return this.afterElmPackage;
    }
    
    var compileOrderKeys = Object.keys(self.internalDeps);
    // Special case for elm-lang/core doesn't include default imports
    if (this.projectSpec.user != "elm-lang" ||
        this.projectSpec.project != "core") {
        Array.prototype.push.apply(compileOrderKeys,defaultImports.map(function(m) { return m.join("."); }));
    }
    var compileOrder = {};
    for (var i = 0; i < compileOrderKeys.length; i++) {
        var k = compileOrderKeys[i];
        if (k.split(".")[0] == "Native") {
            continue;
        }
        if (self.isExternalModule(k)) {
            continue;
        }
        compileOrder[k] = {};
        var imports = self.internalDeps[k].imports || [];
        for (var j = 0; j < imports.length; j++) {
            var imp = imports[j];
            if (imp.split(".")[0] != "Native") {
                compileOrder[k][imp] = true;
            }
        }
    }

    return self._compileOneMore(compileOrder);
}

ElmPackage.prototype._collect = function(compileOrder,m) {
    var self = this;
    if (compileOrder[m]) {
        return;
    }
    var extmod = self.isExternalModule(m);
    if (extmod) {
        return self.packageDeps[extmod]._collect(compileOrder,m);
    }
    compileOrder[m] = compileOrder[m] || {};
    var ideps = self.internalDeps[m];
    var imports = ideps.imports || [];
    for (var i = 0; i < imports.length; i++) {
        var k = imports[i];
        compileOrder[m][k] = true;
        self._collect(compileOrder,k);
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
        var who = self;
        var pname = self.isExternalModule(m);
        if (pname) {
            who = self.packageDeps[pname];
        }
        if (modname[0] == "Native") {
            if (!who.internalDeps[m].js) {
                throw new Error("Could not find js source for"+who.projectSpec+":"+m);
            }
            js = who.internalDeps[m].js;
        } else {
            if (!who.internalDeps[m].elmo) {
                throw new Error("Module "+JSON.stringify(who.projectSpec)+":"+m+" was not compiled");
            }
            js = who.internalDeps[m].elmo;
        }
        result.push("//***"+m,js);
        delete compileOrder[m];
        return self._linkOne(compileOrder,result);
    }
    return q.fcall(function() { return result; });
}

ElmPackage.prototype.link = function(mods) {
    var self = this;
    if (this.elmPackage == null) {
        this.afterElmPackage = this.afterElmPackage.then(function() {
            return self.link(mods);
        });
        return this.afterElmPackage;
    }
    
    var compileOrderKeys = Object.keys(self.internalDeps);
    if (mods) {
        compileOrderKeys.push.apply(compileOrderKeys,mods);
    }
    var compileOrder = {};
    for (var i = 0; i < compileOrderKeys.length; i++) {
        var k = compileOrderKeys[i];
        self._collect(compileOrder, k);
    }

    var result = [];
    console.log("/*Link:",compileOrder,"*/");
    return self._linkOne(compileOrder, result).then(function(result) {
        var fin = [elmStatic.prelude.join("\n")];
        fin.push.apply(fin,result);
        fin.push(elmStatic.footer.join("\n"));
        return fin.join("\n");
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
