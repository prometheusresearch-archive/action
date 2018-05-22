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

build: ppx_let menhirLib
	@esy $(NODE_MODULE_BIN)/bsb -make-world

watch:
	@esy $(NODE_MODULE_BIN)/bsb -make-world -w

test: build
	@$(NODE_MODULE_BIN)/jest --runInBand

test-watch:
	@$(NODE_MODULE_BIN)/jest --watch

clean:
	@esy $(NODE_MODULE_BIN)/bsb -clean
	@rm -rf $(PROJECT_DIR)/lib
	@rm -rf $(PROJECT_DIR)/menhirLib
	@rm -f $(PROJECT_DIR)/ppx_let
