var q = require('q');
var XMLHttpRequest = XMLHttpRequest || require('xmlhttprequest').XMLHttpRequest;
var btoa = btoa || require('btoa');
var elmBasicCompile = require('./elm-basic-compile');

var baseURL = 'http://superheterodyne.net/files/';
var compile = { };

module.exports.init = function() {
    var d = q.defer();
    
    elmBasicCompile.initCompiler(
	function(req) {
	    var requests = req[0];
	    var loaded = req[1];
	    function promiseOneModule(name) {
		var d = q.defer();
		var request = new XMLHttpRequest();
		request.addEventListener('error', function() {
		    d.resolve([name[1], ['error']]);
		});
		request.addEventListener('load', function() {
		    d.resolve([name[1], btoa(request.responseText)]);
		});
		var fileNameGuess = name[0];
		request.open('GET', baseURL + fileNameGuess);
		request.send();
		return d.promise;
	    }
	    q.all(requests.map(promiseOneModule)).then(function(modules) {
		loaded(modules);
	    });
	},
	function(compile) {
	    d.resolve({
		compile: function(source) {
		    var d = q.defer();
		    compile([source, d.resolve]);
		    return d.promise;
		}
	    });
	}
    );

    return d.promise;
}
