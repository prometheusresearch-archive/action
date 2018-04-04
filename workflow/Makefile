include ../Makefile.inc

NODE_MODULE_BIN = $(PROJECT_DIR)/node_modules/.bin

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
	@esy $(NODE_MODULE_BIN)/node_modules/.bin/bsb -make-world

watch:
	@esy $(NODE_MODULE_BIN)/node_modules/.bin/bsb -make-world -w

test:
	@node $(PROJECT_DIR)/src/CoreTest.bs.js
	@node $(PROJECT_DIR)/src/ParsingTest.bs.js
