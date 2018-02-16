bootstrap:
	@esy install
	@esy build
	@$(MAKE) ppx_let
	@$(MAKE) build

ppx_let:
	ln -s $$(esy ocamlfind query ppx_let)/ppx $(@)

build:
	esy ./node_modules/.bin/bsb -make-world

watch:
	esy ./node_modules/.bin/bsb -make-world -w
