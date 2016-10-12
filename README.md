elm-basic-compile
=================

You can build the web demo and the node demo with ```make```.  

The makefile assumes that a cabal sandbox has been initialized in the following way:

    $ cabal sandbox init
    $ cabal sandbox add-source ../elm-compiler # Checked out at 0.17 and patched for aeson-0.8
    $ cabal sandbox add-source .

A gist for the aeson version bump in 0.17 is here:

https://gist.github.com/prozacchiwawa/148611da66eabb7c883995fa9cbd7125

Also make sure to ```npm install``` before building.

After make completes (browserify takes several minutes for me),
index.html can served and will compile elm code.
