var q = require('q');
var pv = require('../js/package-version.js');
var ep = require('../js/elm-package.js');
var gs = require('../js/github-source.js');
var eci = require('../js/elm-compiler-interface.js');
var json = {"elm-lang/core":{"5.0.0": {"version":"5.0.0","summary":"Elm's standard libraries","repository":"http://github.com/elm-lang/core.git","license":"BSD3","source-directories":["src"],"exposed-modules":["Array","Basics","Bitwise","Char","Color","Date","Debug","Dict","Json.Decode","Json.Encode","List","Maybe","Platform","Platform.Cmd","Platform.Sub","Process","Random","Regex","Result","Set","String","Task","Time","Tuple"],"native-modules":true,"dependencies":{},"elm-version":"0.18.0 <= v < 0.19.0"}}};

var packageSpec = {user: "elm-lang", project: "core", version: "5.0.0"};
var epkg = new ep.ElmPackage(new gs.GithubSource(),packageSpec);

eci.init().then(function(compiler) {
    var packageName = pv.packageNameString(packageSpec);
    return epkg.expandPackage(compiler,packageSpec,json[packageName][packageSpec.version]);
}).then(function(reachable) {
    console.log("reachable",Object.keys(reachable));
}).fail(function(e) {
    console.error("Error",e);
});
