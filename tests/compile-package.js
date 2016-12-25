var q = require('q');
var pv = require('../js/package-version.js');
var ep = require('../js/elm-package.js');
var gs = require('../js/github-source.js');
var eci = require('../js/elm-compiler-interface.js');

var packageSpec = {user: "elm-lang", project: "core", version: "5.0.0"};
var epkg = new ep.ElmPackage(new gs.GithubSource(),packageSpec);

eci.init().then(function(compiler) {
    var packageName = pv.packageNameString(packageSpec);
    return epkg.expandPackage(compiler);
}).then(function(reachable) {
    console.log("reachable",Object.keys(reachable));
}).fail(function(e) {
    console.error("Error",e);
});
