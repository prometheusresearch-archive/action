bootstrap:
	@yarn

ui/%:
	@(cd ui && $(MAKE) $(@:ui/%=%))

api/%:
	@(cd api && $(MAKE) $(@:api/%=%))
