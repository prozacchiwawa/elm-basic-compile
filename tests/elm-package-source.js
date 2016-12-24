var GithubSource = require("../js/github-source").GithubSource;
var ElmPackage = require("../js/elm-package").ElmPackage;
//var pkg = { user: "prozacchiwawa", project: "effmodel", version: "2.0.1" };
var pkg = { user: "elm-lang", project: "core", version: "5.0.0" };

var ep = new ElmPackage(new GithubSource(),pkg);
ep.findSourceFiles(["Native","Utils"]).then(function(fl) {
    var types = {};
    for (var i = 0; i < fl.length; i++) {
        var ent = fl[i];
        types[ent[0]] = ent[1];
    }
    return types;
}).then(function(types) {
    if (types.js) {
        console.log("/* js */\n" + types.js);
    }
    if (types.elm) {
        console.log("-- elm --\n" + types.elm);
    }
}).fail(function(e) {
    console.error(e);
});
