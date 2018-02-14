bootstrap:
	@yarn
	@$(MAKE) -s -C workflow bootstrap

ui/%:
	@(cd ui && $(MAKE) $(@:ui/%=%))

api/%:
	@(cd api && $(MAKE) $(@:api/%=%))
