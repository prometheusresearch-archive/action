include ../Makefile.inc

NODE_MODULE_BIN = $(PROJECT_DIR)/node_modules/.bin

bootstrap:
	@yarn

serve-dev:
	@$(NODE_MODULE_BIN)/react-scripts start

check:
	@$(NODE_MODULE_BIN)/flow

.PHONY: build
build:
	@$(NODE_MODULE_BIN)/react-scripts build

deploy:
	@$(NODE_MODULE_BIN)/gh-pages -d build
