bootstrap:
	@esy install
	@esy build
	@$(MAKE) ppx_let menhirLib
	@$(MAKE) build

ppx_let:
	@ln -s $$(esy ocamlfind query ppx_let)/ppx $(@)

menhirLib:
	@cp -rf $$(esy ocamlfind query menhirLib) $(@)

build:
	esy ./node_modules/.bin/bsb -make-world

watch:
	esy ./node_modules/.bin/bsb -make-world -w

test:
	node ./src/CoreTest.bs.js
	node ./src/ParsingTest.bs.js
