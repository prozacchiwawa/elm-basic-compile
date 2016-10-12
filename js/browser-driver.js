var eci = require('./elm-compiler-interface.js');
eci.init().then(function(compiler) {
    var inputCode = document.getElementById('input-code');
    var resultCode = document.getElementById('compiled-code');
    var compileButton = document.getElementById('compile-button');
    inputCode.disabled = false;
    compileButton.disabled = false;
    compileButton.addEventListener('click', function() {
	compiler.compile(inputCode.value).then(function(result) {
	    resultCode.value = result;
	});
    });
});
