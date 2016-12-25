var q = require('q');
var XMLHttpRequest = XMLHttpRequest || require('xmlhttprequest').XMLHttpRequest;
var semver = require("semver");
var ls = require("logic-solver");
var btoa = btoa || require('btoa');
var pv = require("./package-version");

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
    this.buildSolution = null;
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
        this.afterElmPackage = this.afterElmPackage.then(function() {
            return createPromiseForFiles(modname);
        });
        return this.afterElmPackage;
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
    this.solver = new ls.Solver();
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
 */


module.exports.ElmPackage = ElmPackage;
module.exports.PackageSolver = PackageSolver;
