bootstrap:
	@$(MAKE) -C workflow/ bootstrap
	@$(MAKE) -C ui/ bootstrap

watch-workflow:
	@$(MAKE) -C workflow/ watch

serve:
	@$(MAKE) -C ui/ serve-dev
