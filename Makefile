bootstrap:
	@yarn
	@$(MAKE) -s -C workflow bootstrap
	@$(MAKE) -s -C ui bootstrap

ui/%:
	@(cd ui && $(MAKE) $(@:ui/%=%))

api/%:
	@(cd api && $(MAKE) $(@:api/%=%))
