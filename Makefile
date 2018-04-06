bootstrap:
	@$(MAKE) -C core/ bootstrap
	@$(MAKE) -C ui/ bootstrap

watch-core:
	@$(MAKE) -C core/ watch

serve:
	@$(MAKE) -C ui/ serve-dev

test:
	@$(MAKE) -C core/ test

