var q = require('q');
var pv = require('../js/package-version.js');
var ep = require('../js/elm-package.js');
var gs = require('../js/github-source.js');

var packageSpec = {user: "prozacchiwawa", project: "test", version: "1.0.0"};
var retriever = new gs.GithubSource();
retriever.useSourceFile(packageSpec,["Main"],"import Html exposing (text)\nmain =\n  text \"Hi\"");
retriever.useJson(packageSpec,{
    "version": "1.0.0",
    "source-directories": ["src"],
    "repository": "https://github.com/prozacchiwawa/test.git",
    "exposed-modules": [],
    "dependencies": {
        "elm-lang/core": "5.0.0 <= v < 6.0.0",
        "elm-lang/html": "2.0.0 <= v < 3.0.0"
    }    
});
var epkg = new ep.ElmPackage(retriever,packageSpec);
return epkg.expandPackage(["Main"]).then(function(reachable) {
}).then(function() {
    return epkg.compileModule("Main");
}).then(function() {
    return epkg.link(["Main"]);
}).then(function(js) {
    console.log(js);
}).fail(function(e) {
    console.error("Error",e);
});
