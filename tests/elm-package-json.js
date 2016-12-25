var GithubSource = require("../js/github-source").GithubSource;
var epkg = require("../js/elm-package");
var pkg = { user: "prozacchiwawa", project: "test" };
var testJson = {
    "version": "1.0.0",
    "source-directories": ["src"],
    "repository": "https://github.com/prozacchiwawa/test.git",
    "exposed-modules": [],
    "dependencies": {
        "elm-lang/core": "5.0.0 <= v < 6.0.0",
        "elm-lang/html": "2.0.0 <= v < 3.0.0"
    }
};
var ps = new epkg.PackageSolver(new GithubSource());
ps.injectPackage(pkg,testJson);
ps.fillTree(pkg).then(function() {
    console.log("fillTree",JSON.stringify(ps.versions));
}).fail(function(e) {
    console.error("Error:",e);
});
