var q = require('q');
var XMLHttpRequest = XMLHttpRequest || require('xmlhttprequest').XMLHttpRequest;
if (typeof localStorage === "undefined" || localStorage === null) {
  var LocalStorage = require('node-localstorage').LocalStorage;
  localStorage = new LocalStorage('./elm-cache');
}

var lastRequest = (new Date()).getTime();
var requestInterval = 10000;

function promiseOneObject(url,api,headers) {
    var curTime = (new Date()).getTime();
    if (api && lastRequest + requestInterval > curTime) {
        return q.delay(lastRequest + requestInterval - curTime).then(function() {
            return promiseOneObject(url,api,headers);
        });
    }
    lastRequest = curTime;
    var d = q.defer();
    var request = new XMLHttpRequest();
    request.addEventListener('error', function() {
        console.error(request.status,request.responseText);
        d.reject({status: request.status, text: "could not load " + name});
    });
    request.addEventListener('load', function() {
        if (request.status != 200) {
            console.error("Error", request.status,request.responseText, url);
            d.reject({status: request.status, text: request.responseText});
        } else {
            console.log("Success", url);
            d.resolve(request.responseText);
        }
    });
    request.open('GET', url);
    if (headers) {
        for (var k in headers) {
            request.setRequestHeader(k, headers[k]);
        }
    }
    request.send();
    return d.promise;
}

function GithubSource() {
}
/* Return a promise for elm-package.json given a projectSpec */
GithubSource.prototype.retrieveJson = function(projectSpec) {
    var url = "https://raw.githubusercontent.com/" + projectSpec.user + "/" + projectSpec.project + "/" + projectSpec.version + "/elm-package.json";
    var item = localStorage.getItem(url);
    if (item) {
        return q.fcall(function() { return item; });
    } else {
        return promiseOneObject(url,false).then(function(json) {
            localStorage.setItem(url, json);
            return json;
        });
    }
}
/*
 * Return a promise for a source file from a published elm project given
 * a package spec, a possible source directory and a module name (list of
 * string).  ext is the file extension to be retrieved.
 */
GithubSource.prototype.retrieveSource = function(projectSpec,srcDir,modname,ext) {
    return promiseOneObject("https://raw.githubusercontent.com/" + projectSpec.user + "/" + projectSpec.project + "/" + projectSpec.version + "/" + srcDir + "/" + modname.join("/") + "." + ext,false);
}
/*
 * Return a list of packageSpec given a project name.
 * projectName like {user: "prozacchiwawa", project: "effmodel"}
 * Promises a list of tag name.
 */
GithubSource.prototype.retrieveTags = function(projectName) {
    var url = "https://api.github.com/repos/" + projectName.user + "/" + projectName.project + "/tags";
    var item = localStorage.getItem(url);
    if (item) {
        console.log("cache",JSON.parse(item));
        return q.fcall(function() { return JSON.parse(item); });
    } else {
        var params = {"User-Agent": "elm-basic-compile", "Accept": "application/json"};
        var token = localStorage.getItem("apikey");
        if (token) {
            params.Authorization = "token " + token;
        }
        return promiseOneObject(url, true, params).then(function(tags) {
            return JSON.parse(tags);
        }).then(function(tags) {
            return tags.map(function(tag) { return tag.name; });
        }).then(function(tags) {
            localStorage.setItem(url, JSON.stringify(tags));
            return tags;
        });
    }
}

module.exports.GithubSource = GithubSource;
