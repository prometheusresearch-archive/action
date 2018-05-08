include ../Makefile.inc

NODE_MODULE_BIN = $(PROJECT_DIR)/node_modules/.bin

bootstrap:
	@yarn

serve:
	@$(NODE_MODULE_BIN)/next

deploy:
	@$(NODE_MODULE_BIN)/gh-pages -d out
