var eci = require('./elm-compiler-interface');
eci.init().then(function(compiler) {
    compiler.compile('x = 3').then(function(js) {
	console.log('/*js*/' + js);
    });
});
