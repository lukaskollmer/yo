# Shamelessly stolen from https://owensd.io/2015/01/13/compiling-swift-without-xcode/

# A simple build script for building projects.
#
# usage: make [CONFIG=debug|release]

.PHONY: build

CONFIG     ?= Debug

ROOT_DIR    = $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
OUTPUT_DIR  = $(ROOT_DIR)/build

XCODEBUILD  = $(shell xcrun -f xcodebuild)
XCPRETTY    = $(shell which xcpretty)
TARGET_NAME = yo
CLOC        = $(shell which cloc)

build:
	$(XCODEBUILD) -target $(TARGET_NAME) SYMROOT=$(OUTPUT_DIR) -configuration $(CONFIG) build | $(XCPRETTY)

cloc:
	$(CLOC) . --force-lang="Rust",yo --exclude-dir=bin,deps

todo:
	grep -ri --exclude-dir=.git 'todo' .

clean:
	rm -rf $(OUTPUT_DIR)
