MAKEFILE_PATH := $(abspath $(firstword $(MAKEFILE_LIST)))
PROJECT_DIR := $(shell dirname $(MAKEFILE_PATH))
