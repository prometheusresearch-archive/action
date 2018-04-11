define HELP
Welcome to action development repository!

Common tasks:

- make bootstrap - run bootstrap process
- make serve - serve demo app
- make test - run test suite

Start with "make bootstrap" if not sure. Have fun hacking on action!
endef
export HELP

help:
	@echo "$$HELP"

bootstrap:
	@$(MAKE) -C core/ bootstrap
	@$(MAKE) -C ui/ bootstrap
	@$(MAKE) -C components/ bootstrap

watch-core:
	@$(MAKE) -C core/ watch

serve:
	@$(MAKE) -C ui/ serve

serve-components:
	@$(MAKE) -C ui/ serve

test:
	@$(MAKE) -C core/ test

