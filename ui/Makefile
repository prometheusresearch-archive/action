BIN = $(PWD)/../node_modules/.bin

bootstrap:
	@(cd node_modules && ln -s ../../workflow .)

serve-dev:
	@$(BIN)/react-scripts start

check:
	@$(BIN)/flow
