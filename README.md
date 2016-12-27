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

See tests/link-program.js for a slightly richer interface.
