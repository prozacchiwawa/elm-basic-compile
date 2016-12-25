var semver = require("semver");

function packageNameString(packageName,version) {
    return packageName.user + "/" + packageName.project;
}

function parsePackageName(pname) {
    plist = pname.split("/");
    return { user: plist[0], project: plist[1] };
}

function getAllowedVersionsOfPackage(vtree,pkg,dep) {
    /* Each package that requires this package */
    var pname = pkg;
    var match = dep.match(/([0-9\.]+)[ ]*<=[ ]*v[ ]*<[ ]*([0-9\.]+)$/);
    if (!match) {
        throw new Exception("Can't interpret dep entry " + dep + " for " + j);
    }
    /* match[1] <= v < match[2] */
    var lowerbound = match[1];
    var upperbound = match[2];
    if (!semver.valid(lowerbound)) {
        throw new Exception("Can't interpret lower bound version " + lowerbound);
    }
    if (!semver.valid(upperbound)) {
        throw new Exception("Can't interpret upper bound version " + upperbound);
    }
    var vers = Object.keys(vtree[pname]);
    var res = [];
    for (var i = 0; i < vers.length; i++) {
        var v = vers[i];
        if (!semver.valid(v)) {
            throw new Exception("Can't parse new version " + v)
        }
        if (semver.lte(lowerbound,v) && semver.lt(v,upperbound)) {
            res.push(v);
        }
    }
    return res;
}

function getNotAllowedVersionsOfPackage(vtree,pkg,versions) {
    var pname = pkg;
    var allversions = vtree[pname];
    var verkeys = Object.keys(allversions);
    for (var i = 0; i < versions.length; i++) {
        var ver = versions[i];
        var idx = verkeys.indexOf(ver);
        if (idx > -1) {
            verkeys.splice(idx, 1);
        }
    }
    return verkeys;
}

module.exports.packageNameString = packageNameString;
module.exports.parsePackageName = parsePackageName;
module.exports.getAllowedVersionsOfPackage = getAllowedVersionsOfPackage;
module.exports.getNotAllowedVersionsOfPackage = getNotAllowedVersionsOfPackage;
