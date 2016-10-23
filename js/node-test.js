var eci = require('./elm-compiler-interface');
eci.init().then(function(compiler) {
    compiler.compile('import Html exposing (text)\n\nmain = text "Hello"').then(function(js) {
	console.log('/*js*/' + js);
    });
});
