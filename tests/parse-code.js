var eci = require('../js/elm-compiler-interface.js');
eci.init().then(function(compiler) {
    console.log("compiler",compiler);
    compiler.parse(["prozacchiwawa","test"], "x = 3").then(function(res) {
        console.log(res);
    });
});
