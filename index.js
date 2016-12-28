var q = require('q');
var pv = require('./js/package-version');
var ep = require('./js/elm-package');
var gs = require('./js/github-source');

module.exports.packageNameString = pv.packageNameString;
module.exports.parsePackageName = pv.parsePackageName;
module.exports.GithubSource = gs.GithubSource;
module.exports.ElmPackage = ep.ElmPackage;
module.exports.PackageSolver = ep.PackageSolver;

/* The simplest possible compilation interface. */
module.exports.justCompile = function(source) {
    var packageSpec = {user: "prozacchiwawa", project: "test", version: "1.0.0"};
    var retriever = new gs.GithubSource();
    retriever.useSourceFile(packageSpec,["Main"],source);
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
        return epkg.compileModule("Main",true);
    }).then(function() {
        return epkg.link(["Main"]);
    }).fail(function(e) {
        console.error(e);
        return {error: e};
    });
}
