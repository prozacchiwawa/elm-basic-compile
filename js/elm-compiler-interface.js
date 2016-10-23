var q = require('q');
var XMLHttpRequest = XMLHttpRequest || require('xmlhttprequest').XMLHttpRequest;
var btoa = btoa || require('btoa');
var elmBasicCompile = require('./elm-basic-compile');

var baseURL = 'http://superheterodyne.net/files/';
var compile = { };

var deps = [
  [["elm-lang", "core", ["Native", "Utils"]], "4.0.0"],
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

var jsprelude = [
  "'use strict';",
  "function F2(fun)",
  "{",
  "function wrapper(a) { return function(b) { return fun(a,b); }; }",
  "wrapper.arity = 2;",
  "wrapper.func = fun;",
  "return wrapper;",
  "}",
  "",
  "function F3(fun)",
  "{",
  "function wrapper(a) {",
  "return function(b) { return function(c) { return fun(a, b, c); }; };",
  "}",
  "wrapper.arity = 3;",
  "wrapper.func = fun;",
  "return wrapper;",
  "}",
  "",
  "function F4(fun)",
  "{",
  "function wrapper(a) { return function(b) { return function(c) {",
  "return function(d) { return fun(a, b, c, d); }; }; };",
  "}",
  "wrapper.arity = 4;",
  "wrapper.func = fun;",
  "return wrapper;",
  "}",
  "",
  "function F5(fun)",
  "{",
  "function wrapper(a) { return function(b) { return function(c) {",
  "return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };",
  "}",
  "wrapper.arity = 5;",
  "wrapper.func = fun;",
  "return wrapper;",
  "}",
  "",
  "function F6(fun)",
  "{",
  "function wrapper(a) { return function(b) { return function(c) {",
  "return function(d) { return function(e) { return function(f) {",
  "return fun(a, b, c, d, e, f); }; }; }; }; };",
  "}",
  "wrapper.arity = 6;",
  "wrapper.func = fun;",
  "return wrapper;",
  "}",
  "",
  "function F7(fun)",
  "{",
  "function wrapper(a) { return function(b) { return function(c) {",
  "return function(d) { return function(e) { return function(f) {",
  "return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };",
  "}",
  "wrapper.arity = 7;",
  "wrapper.func = fun;",
  "return wrapper;",
  "}",
  "",
  "function F8(fun)",
  "{",
  "function wrapper(a) { return function(b) { return function(c) {",
  "return function(d) { return function(e) { return function(f) {",
  "return function(g) { return function(h) {",
  "return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };",
  "}",
  "wrapper.arity = 8;",
  "wrapper.func = fun;",
  "return wrapper;",
  "}",
  "function F9(fun)",
  "{",
  "function wrapper(a) { return function(b) { return function(c) {",
  "return function(d) { return function(e) { return function(f) {",
  "return function(g) { return function(h) { return function(i) {",
  "return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };",
  "}",
  "wrapper.arity = 9;",
  "wrapper.func = fun;",
  "return wrapper;",
  "}",
  "",
  "function A2(fun, a, b)",
  "{",
  "return fun.arity === 2",
  "? fun.func(a, b)",
  ": fun(a)(b);",
  "}",
  "function A3(fun, a, b, c)",
  "{",
  "return fun.arity === 3",
  "? fun.func(a, b, c)",
  ": fun(a)(b)(c);",
  "}",
  "function A4(fun, a, b, c, d)",
  "{",
  "return fun.arity === 4",
  "? fun.func(a, b, c, d)",
  ": fun(a)(b)(c)(d);",
  "}",
  "function A5(fun, a, b, c, d, e)",
  "{",
  "return fun.arity === 5",
  "? fun.func(a, b, c, d, e)",
  ": fun(a)(b)(c)(d)(e);",
  "}",
  "function A6(fun, a, b, c, d, e, f)",
  "{",
  "return fun.arity === 6",
  "? fun.func(a, b, c, d, e, f)",
  ": fun(a)(b)(c)(d)(e)(f);",
  "}",
  "function A7(fun, a, b, c, d, e, f, g)",
  "{",
  "return fun.arity === 7",
  "? fun.func(a, b, c, d, e, f, g)",
  ": fun(a)(b)(c)(d)(e)(f)(g);",
  "}",
  "function A8(fun, a, b, c, d, e, f, g, h)",
  "{",
  "return fun.arity === 8",
  "? fun.func(a, b, c, d, e, f, g, h)",
  ": fun(a)(b)(c)(d)(e)(f)(g)(h);",
  "}",
  "function A9(fun, a, b, c, d, e, f, g, h, i)",
  "{",
  "return fun.arity === 9",
  "? fun.func(a, b, c, d, e, f, g, h, i)",
  ": fun(a)(b)(c)(d)(e)(f)(g)(h)(i);",
  "}"
];

module.exports.init = function() {
    var d = q.defer();

    elmBasicCompile.initCompiler(
      	[deps, graph],
      	function(req) {
      		var requests = req[0];
      		var loaded = req[1];
      		function promiseOneObject(name) {
      			var d = q.defer();
      			if (name == '') {
      				d.resolve('');
      				return d.promise;
      			}
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
            modules[0] += jsprelude.join('\n');
      			loaded(modules);
      		});
      	},
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
