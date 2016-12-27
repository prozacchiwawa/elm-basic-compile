all: \
	.cabal-sandbox/bin/elm-basic-compile.jsexe/out.js \
	js/elm-basic-compile.js

elm-compiler:
	git clone https://github.com/elm-lang/elm-compiler.git
	cd elm-compiler && git checkout 0.18.0 && patch -p1 < ../patch/elm-compiler.diff

elm-package:
	git clone https://github.com/elm-lang/elm-package.git
	cd elm-package && git checkout 0.18.0 && patch -p1 < ../patch/elm-package.diff

.cabal-sandbox: elm-compiler elm-package
	cabal sandbox init
	cabal sandbox add-source .
	cabal sandbox add-source elm-compiler
	cabal sandbox add-source elm-package

.cabal-sandbox/bin/elm-basic-compile.jsexe/out.js: .cabal-sandbox src/*.hs
	cabal install -j elm-basic-compile

js/elm-basic-compile.js: src/*.hs js/export.js
	cat \
		js/prelude.js \
		.cabal-sandbox/bin/elm-basic-compile.jsexe/rts.js \
		.cabal-sandbox/bin/elm-basic-compile.jsexe/lib.js \
		.cabal-sandbox/bin/elm-basic-compile.jsexe/out.js \
		js/export.js \
		> $@

clean:
	rm -rf .cabal-sandbox
