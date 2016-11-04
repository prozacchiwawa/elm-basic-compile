window.addEventListener('message', function(msg) {
    if (msg.data.msg && msg.data.msg === 'elm-compiler') {
        var inputCode = document.getElementById('input-code');
        var resultCode = document.getElementById('compiled-code');
        var compileButton = document.getElementById('compile-button');
        inputCode.disabled = false;
        compileButton.disabled = false;
        compileButton.addEventListener('click', function() {
            window.compile(inputCode.value, function(result) {
	            resultCode.value = result;
	        });
        });
    }
});
