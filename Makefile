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

