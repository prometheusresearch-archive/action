BIN = $(PWD)/node_modules/.bin

bootstrap:
	@yarn

serve-dev:
	@$(BIN)/react-scripts start

check:
	@$(BIN)/flow

.PHONY: build
build:
	@$(BIN)/react-scripts build

deploy:
	@$(BIN)/gh-pages -d build
