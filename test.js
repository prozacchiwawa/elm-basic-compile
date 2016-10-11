var elmBasicCompile = require('./elm-basic-compile');
elmBasicCompile.initCompiler(
    function() {
	console.log('loadModule', Array.prototype.slice(arguments));
    },
    function(compile) {
	console.log('callback', Array.prototype.slice(arguments));
	var compiledCallback = function(js) {
	    console.log('compiler result',js);
	}
	compile(["x = 3", compiledCallback]);
    }
);
