all: elm-basic-compile.js

elm-basic-compile.js: src/*.hs export.js
	cabal install -j elm-basic-compile
	cat \
		.cabal-sandbox/bin/elm-basic-compile.jsexe/rts.js \
		.cabal-sandbox/bin/elm-basic-compile.jsexe/lib.js \
		.cabal-sandbox/bin/elm-basic-compile.jsexe/out.js \
		export.js \
		> $@

