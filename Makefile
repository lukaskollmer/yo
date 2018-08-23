# github.com/lukaskollmer/yo

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
	$(CLOC) $(ROOT_DIR) --force-lang="Rust",yo --exclude-dir=build,deps

todo:
	grep -ri --exclude-dir=.git 'todo' .

clean:
	rm -rf $(OUTPUT_DIR)
