elm-basic-compile
=================

An npm module which incorporates the elm-compiler built with GHCJS and presenting a simplified compilation interface.  It is currently very rough, not well tested, and ripe for improvement.

The simple way to use it is like this:

    var ebc = require('elm-basic-compile');
    ebc.justCompile("import Html\nmain=Html.text \"hi\"").then(function(res) {
        if (res.error) {
            // error
        } else {
            // eval(res); // Javascript should be runnable.
        }
    });

More details
------------

The builder uses the same file formats as elm-make does, basically just elm-package.json and elm source files.
You can use javascript inputs as well as Native modules in the usual way.

The builder has two parts:

a ```Retriever``` object which gets gets source code and json files.  The default one, GithubSource, retrieves
needed source files and version sets from github, as elm-make does, but does not strictly require that packages
are acceptable by elm-package, and one could make their own or extend this one to support different sources.
It has this shape:

    type ProjectName = { user: string, project : string }
    type ProjectSpec = { user: string, project: string, version: string }
    
    interface Retriever {
        // Retrieves the version numbers of the package
        retrieveTags(projectName : ProjectName) : Promise<[string]>
        // Retrieves the elm-package.json of the package
        retrieveJson(projectSpec : ProjectSpec) : Promise<string>
        // Retrieves the indicated source file from the package.
        // Errors must be reported as {status: int, text: info} , and 404 is treated
        // as a non-fatal error since there might be multiple source directories in
        // an elm package.
        retrieveSource(projectSpec : ProjectSpec, srcDir : string, modname : [string], ext : string) : Promise<string>
    }
    
The default retriever in this module is ```GithubSource``` which has some extra methods that allow you to put
files in it for the compiler to use:

    useJson(projectSpec : ProjectSpec, json : (string | dict)) : Unit
    useSourceFile(projectSpec : ProjectSpec, modname : [string], content : string) : Unit

The compiler driver is called ```ElmPackage``` which has this shape:

    class ElmPackage {
        constructor(retriever : Retriever, projectSpec : ProjectSpec)
        // Turn spammy diagnostics on or off
        debug(on : bool) : Unit
        // Ensure that dependencies are present in the cache for the exposed-modules
        // of the package plus any modules listed in the optional force argument.
        expandPackage(force : [string]?) : Promise<Unit>
        // Compile beginning with the given Main module.
        // If force is specified, the result will be cached in the object but not in
        // localStorage, and localStorage won't be queried for that module.
        compileModule(modname : string, force : bool) : Promise<Unit,{error: Error}>
        // Link the package, producing executable javascript.
        // targets specifies the modules that begin dependency analysis.
        link(targets : [string]) : Promise<String,Unit>
    }
    
Usage example
-------------

This is from ```index.js``` from elm-basic-compile

    /* The simplest possible compilation interface. */
    module.exports.justCompile = function(source) {
        var packageSpec = {user: "prozacchiwawa", project: "test", version: "1.0.0"};
        var retriever = new gs.GithubSource();
        retriever.useSourceFile(packageSpec,["Main"],source);
        retriever.useJson(packageSpec,{
            "version": "1.0.0",
	        "source-directories": ["src"],
            "repository": "https://github.com/prozacchiwawa/test.git",
            "exposed-modules": [],
            // If you want to compile with more dependencies, specify them here.
            // You could make your own retriever that would have more sources than
            // github.  Also this retriever doesn't require that the packages are
            // accepted by elm-package.
            "dependencies": {
                "elm-lang/core": "5.0.0 <= v < 6.0.0",
                "elm-lang/html": "2.0.0 <= v < 3.0.0"
            }
        });
        var epkg = new ep.ElmPackage(retriever,packageSpec);
        return epkg.expandPackage(["Main"]).then(function(reachable) {
            return epkg.compileModule("Main",true);
        }).then(function() {
            return epkg.link(["Main"]);
        }).fail(function(e) {
            console.error(e);
	        return {error: e};
        });
    } // -> The result is a Promise<string,{error: Error}>, which gives executable
      // javascript or an error from the elm compiler.
