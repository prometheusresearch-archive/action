include ../Makefile.inc

NODE_MODULE_BIN = $(PROJECT_DIR)/node_modules/.bin

bootstrap:
	@esy
	@$(MAKE) menhirLib
	@$(MAKE) build

menhirLib:
	@cp -rf $$(esy ocamlfind query menhirLib) $(@)

build:
	@esy $(NODE_MODULE_BIN)/bsb -make-world

watch:
	@esy $(NODE_MODULE_BIN)/bsb -make-world -w

test: build
	@$(NODE_MODULE_BIN)/jest --runInBand

test-watch:
	@$(NODE_MODULE_BIN)/jest --watch
