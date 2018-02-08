BIN = $(PWD)/../node_modules/.bin

serve-dev:
	@./node_modules/.bin/babel-node ./boot.js

check:
	@$(BIN)/flow

test:
	@$(BIN)/jest

test-watch:
	@$(BIN)/jest --watch
