var q = require('q');
var XMLHttpRequest = XMLHttpRequest || require('xmlhttprequest').XMLHttpRequest;
var btoa = btoa || require('btoa');
var elmBasicCompile = require('./elm-basic-compile');

var baseURL = 'http://superheterodyne.net/files/';
var compile = { };

var deps = [
  [["elm-lang", "core", ["Array"]], "4.0.0"],
  [["elm-lang", "core", ["Basics"]], "4.0.0"],
  [["elm-lang", "core", ["Debug"]], "4.0.0"],
  [["elm-lang", "core", ["Dict"]], "4.0.0"],
  [["elm-lang", "core", ["Result"]], "4.0.0"],
  [["elm-lang", "core", ["List"]], "4.0.0"],
  [["elm-lang", "core", ["Maybe"]], "4.0.0"],
  [["elm-lang", "core", ["Json","Decode"]], "4.0.0"],
  [["elm-lang", "core", ["Json","Encode"]], "4.0.0"],
  [["elm-lang", "core", ["Platform"]], "4.0.0"],
  [["elm-lang", "core", ["Platform","Cmd"]], "4.0.0"],
  [["elm-lang", "core", ["Platform","Sub"]], "4.0.0"],
  [["elm-lang", "virtual-dom", ["VirtualDom"]], "1.0.0"],
  [["elm-lang", "html", ["Html"]], "1.0.0"]
];

var graph = [
  [["Basics"], []],
  [["Debug"], [["Basics"]]],
  [["Maybe"], [["Basics"]]],
  [["Result"], [["Maybe"]]],
  [["List"], [["Maybe"]]],
  [["Array"], [["List"]]],
  [["Dict"], [["Basics"], ["List"]]],
  [["Platform"], [["List"]]],
  [["Json","Decode"], [["Maybe"]]],
  [["Json","Encode"], [["Maybe"]]],
  [["Platform", "Cmd"], [["Platform"], ["Json","Decode"], ["Json", "Encode"]]],
  [["Platform", "Sub"], [["Platform"], ["Json","Decode"], ["Json", "Encode"]]],
  [["VirtualDom"], [["Platform"], ["Platform", "Sub"], ["Platform", "Cmd"], ["List"], ["Maybe"]]],
  [["Html"], [["VirtualDom"]]]
];

module.exports.init = function() {
    var d = q.defer();

    elmBasicCompile.initCompiler(
	[deps, graph],
	function(req) {
		var requests = req[0];
		console.log('load interfaces', requests);
		var loaded = req[1];
		function promiseOneObject(name) {
			var d = q.defer();
			if (name == '') {
				d.resolve('');
				return d.promise;
			}
			console.log('promiseOneObject', name);
			var request = new XMLHttpRequest();
			request.addEventListener('error', function() {
			    d.resolve('throw "could not load ' +name+ '";');
			});
			request.addEventListener('load', function() {
			    d.resolve(request.responseText);
			});
			var fileNameGuess = name;
			var url = baseURL + fileNameGuess;
			request.open('GET', url);
			request.send();
			return d.promise;
		}
		q.all(requests.map(promiseOneObject)).then(function(modules) {
			console.log('/* all modules */', modules);
			loaded(modules);
		});
	},
	function(req) {
	    var requests = req[0];
		console.log('load objs', requests);
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
