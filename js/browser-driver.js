var eci = require('./elm-compiler-interface.js');
eci.init().then(function(compiler) {
    window.postMessage({msg: 'elm-compiler'}, window.location.href);
    window.compile = function(source,next) {
        compiler.compile(source).then(function(result) {
            next(result);
        });
    };
});
