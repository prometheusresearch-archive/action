watch:
	esy ./node_modules/.bin/bsb -w

bootstrap:
	esy build
	ln -s $$(esy ocamlfind query ppx_let)/ppx ./ppx_let
