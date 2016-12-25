var q = require('q');
var pv = require('../js/package-version.js');
var ep = require('../js/elm-package.js');
var gs = require('../js/github-source.js');
var eci = require('../js/elm-compiler-interface.js');
var json = {"elm-lang/core":{"5.0.0": {"version":"5.0.0","summary":"Elm's standard libraries","repository":"http://github.com/elm-lang/core.git","license":"BSD3","source-directories":["src"],"exposed-modules":["Array","Basics","Bitwise","Char","Color","Date","Debug","Dict","Json.Decode","Json.Encode","List","Maybe","Platform","Platform.Cmd","Platform.Sub","Process","Random","Regex","Result","Set","String","Task","Time","Tuple"],"native-modules":true,"dependencies":{},"elm-version":"0.18.0 <= v < 0.19.0"}}};

var packageSpec = {user: "elm-lang", project: "core", version: "5.0.0"};
var epkg = new ep.ElmPackage(new gs.GithubSource(),packageSpec);
var importsRe = /^\/\/[ ]*import[ ]+([^\/\n\r]*)\/\//m;
var wsRe = /^\s+|\s+$/g;

function compilePackageFullyStep(compiler,versions,reachableSet,packageSpec,exposed) {
    return q.all(exposed.map(function(mod) {
        var modname = mod.split('.');
        return epkg.findSourceFiles(mod.split(".")).then(function(sf) {
            var s = {name: mod};
            for (var i = 0; i < sf.length; i++) {
                var ty = sf[i][0];
                var sd = sf[i][1];
                if (sd) {
                    s[ty] = sd;
                }
            }
            return s;
        });
    })).then(function(sources) {
        return q.all(sources.map(function(s) {
            if (reachableSet[s.name]) {
                return s;
            }
            reachableSet[s.name] = s;
            if (s.js) {
                var jsImport = s.js.match(importsRe);
                if (jsImport != null) {
                    var importsStrip = jsImport[1].replace(wsRe, '');
                    s.imports = importsStrip.split(',').map(function(i) {
                        return i.replace(wsRe, '');
                    }).filter(function(imp) {
                        return imp !== '';
                    }).filter(function(imp) {
                        return !reachableSet[imp];
                    });
                    return compilePackageFullyStep(compiler,versions,reachableSet,packageSpec,s.imports);
                }
            } else if (s.elm) {
                return compiler.parse(s.name, s.elm).then(function(res) {
                    s.imports = res[1].map(function(mod) {
                        return mod.join(".");
                    }).filter(function(imp) {
                        return !reachableSet[imp];
                    });
                    return compilePackageFullyStep(compiler,versions,reachableSet,packageSpec,s.imports);
                });
            } else {
                s.imports = [];
                return reachableSet;
            }
        })).then(function() { return reachableSet; });
    });
}

function compilePackageFully(compiler,versions,packageSpec) {
    var packageName = pv.packageNameString(packageSpec);
    var packageVersions = versions[packageName];
    var json = packageVersions[packageSpec.version];
    var exposed = json["exposed-modules"];
    var reachableSet = {};
    return compilePackageFullyStep(compiler,versions,reachableSet,packageSpec,exposed);
}

eci.init().then(function(compiler) {
    return compilePackageFully(compiler,json,packageSpec);
}).then(function(reachable) {
    console.log("reachable",Object.keys(reachable));
}).fail(function(e) {
    console.error("Error",e);
});
