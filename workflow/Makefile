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
	@esy $(NODE_MODULE_BIN)/bsb -make-world

watch:
	echo $(PROJECT_DIR)
	echo $(NODE_MODULE_BIN)
	esy $(NODE_MODULE_BIN)/bsb -make-world -w

test: build
	@$(NODE_MODULE_BIN)/jest

test-watch:
	@$(NODE_MODULE_BIN)/jest --watch
