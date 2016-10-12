all: js/elm-basic-compile.js js/browsertest.js

js/elm-basic-compile.js: src/*.hs js/export.js
	cabal install -j elm-basic-compile
	cat \
		js/prelude.js \
		.cabal-sandbox/bin/elm-basic-compile.jsexe/rts.js \
		.cabal-sandbox/bin/elm-basic-compile.jsexe/lib.js \
		.cabal-sandbox/bin/elm-basic-compile.jsexe/out.js \
		js/export.js \
		> $@


js/browsertest.js: \
	js/elm-basic-compile.js \
	js/elm-compiler-interface.js \
	js/browser-driver.js
	browserify -o $@ js/browser-driver.js
