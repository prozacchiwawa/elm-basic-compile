var q = require('q');
var pv = require('../js/package-version.js');
var ep = require('../js/elm-package.js');
var gs = require('../js/github-source.js');

var packageSpec = {user: "elm-lang", project: "core", version: "5.0.0"};
var epkg = new ep.ElmPackage(new gs.GithubSource(),packageSpec);

var packageName = pv.packageNameString(packageSpec);
return epkg.expandPackage().then(function(reachable) {
    console.log("reachable",Object.keys(reachable));
    return epkg.compile(compiler);
}).then(function(res) {
    console.log("compile",res);
}).fail(function(e) {
    console.error("Error",e);
});
