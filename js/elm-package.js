var q = require('q');
var XMLHttpRequest = XMLHttpRequest || require('xmlhttprequest').XMLHttpRequest;
var semver = require("semver");
var btoa = btoa || require('btoa');
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
function ElmPackage(retriever,projectSpec) {
    this.projectSpec = projectSpec;
    this.retriever = retriever;
    this.elmPackage = null;
    var self = this;
    this.afterElmPackage = retriever.retrieveJson(projectSpec).then(function(pspec) {
        self.elmPackage = JSON.parse(pspec);
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
            results.push(tryOneSourceDir(sourceDir,modname,"elm").
                then(function(file) { return ["elm",file]; }));
            results.push(tryOneSourceDir(sourceDir,modname,"js").
                then(function(file) { return ["js",file]; }));
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
        return this.afterElmPackage.then(function() {
            return createPromiseForFiles(modname);
        });
    } else {
        return createPromiseForFiles(modname);
    }
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
    this.versions = {};
}

PackageSolver.prototype.packageNameString = function(packageName,version) {
    return packageName.user + "/" + packageName.project;
}

PackageSolver.prototype.parsePackageName = function(pname) {
    plist = pname.split("/");
    return { user: plist[0], project: plist[1] };
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
    var pname = this.packageNameString(packageSpec);
    this.versions[pname] = this.versions[pname] || {};
    this.versions[pname][json.version] = json;
}

/*
 * Promise a filled dep tree for a specific package.
 */
PackageSolver.prototype.fillTree = function(packageSpec) {
    console.log("fillTree",packageSpec);
    var simpleName = this.packageNameString(packageSpec);
    console.log("simpleName",simpleName);
    console.log("versions",JSON.stringify(this.versions[simpleName]));
    var vset = Object.keys(this.versions[simpleName]);
    var self = this;
    return q.all(vset.map(function(v) {
        return self._promiseDeps(self.versions[simpleName][v]);
    }));
}

/* Given a package and another package that requires it, return allowed
 * versions from an already constrained set. */
PackageSolver.prototype._narrowVersionsOfPackage = function(pname,versions,pkgWantsUs) {
    /* Each package that requires this package */
    var wantingJson = this.packageSet[pkgWantsUs];
    for (var j in wantingJson["dependencies"]) {
        var dep = '' + wantingJson[j];
        var match = dep.match(/^([0-9\.])[ ]*<=[ ]*v[ ]*<[ ]*([0-9\.])$/);
        if (!match) {
            throw new Exception("Can't interpret dep entry " + dep + " for " + j);
        }
        /* match[0] <= v < match[1] */
        var lowerbound = match[0];
        var upperbound = match[1];
        if (!semver.valid(lowerbound)) {
            throw new Exception("Can't interpret lower bound version " + lowerbound);
        }
        if (!semver.valid(upperbund)) {
            throw new Exception("Can't interpret upper bound version " + upperbound);
        }
        versions = versions.filter(function(v) {
            if (!semver.valid(v)) {
                throw new Exception("Can't parse new version " + v)
            }
            return semver.lte(lowerbound,v) && semver.lt(v,upperbound);
        });
    }
    return versions;
}

/* Promise all dependencies to be retrieved. */
PackageSolver.prototype._promiseDeps = function(json) {
    console.log("promiseDeps",json.repository);
    var self = this;
    var depkeys = Object.keys(json.dependencies);
    var wantdeps = depkeys.filter(function(k) { return !self.versions[k]; });
    console.log("wantdeps",wantdeps);
    return q.all(wantdeps.map(function(k) {
        var reqspec = self.parsePackageName(k);
        return self.retriever.retrieveTags(reqspec).then(function(tags) {
            console.log("tags",tags);
            return q.all(tags.map(function(v) {
                var pspec = self.parsePackageName(k);
                pspec.version = v;
                return self.retriever.retrieveJson(pspec).then(function(j) {
                    var json = JSON.parse(j);
                    self.versions[k] = self.versions[k] || {};
                    self.versions[k][v] = json;
                    return json;
                });
            }));
        }).then(function() {
            var pspec = self.parsePackageName(k);
            return self.fillTree(pspec);
        });
    }));
}

/* Solve for a workable package configuration.
 * Yields a map of package name to the latest matching version. */
PackageSolver.prototype.solve = function(packageName) {
    /* If we don't have at least this package, then fail. */
    var pname = this.packageNameString(packageName);
    if (!this.packageSet[pname]) {
        throw new Exception("Unknown package " + pname);
    }
}

function ElmProgram(packageRepoRequest,rootPackage,sourceModules) {
    this.rootPackage = rootPackage;
    this.inputSources = { };
    this.packages = { };
    var self = this;
    this.afterLoad = q.all(sourceModules.map(function(sm) {
        self.rootPackage.findSourceFiles(sm).then(function(sources) {
            var next = self.inputSources;
            for (var i = 0; i < sources.length; i++) {
                var modname = sm[i];
                var sourceres = sources[i];
                next[modname.join(".")] = sourceres;
            }
            return next;
        })
    })).then(function(sources) {
        for (var i = 0; i < self.rootPackage.elmPackage.length; i++) {
            var rp = self.rootPackage.elmPackage[i];
            
        }
    })
}

ElmProgram.loadPackage = function(pkgname) {
    
}

module.exports.ElmPackage = ElmPackage;
module.exports.ElmProgram = ElmProgram;
module.exports.PackageSolver = PackageSolver;
