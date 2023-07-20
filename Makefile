.PHONY: default submodule build bundle minify clean

default: build bundle

submodule reflex-platform/default.nix:
	git submodule update --init

build: reflex-platform/default.nix
	nix-build -A ghcjs.frontend

bundle:
	./tools/bundle.sh result-bundled

minify:
	mv --force result-bundled/all.js result-bundled/all.orig.js
	closure-compiler \
		--compilation_level=ADVANCED_OPTIMIZATIONS \
		--externs=result/bin/frontend.jsexe/all.js.externs \
		--jscomp_off=checkVars \
		--js_output_file="result-bundled/all.js" \
		--warning_level=QUIET \
		result-bundled/all.orig.js
	rm --force result-bundled/all.orig.js

clean:
	rm --force result
	rm --force --recursive result-bundled
