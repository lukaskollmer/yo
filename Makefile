# NOTE: this is broken. just use the xcode project instead

# Shamelessly stolen from https://owensd.io/2015/01/13/compiling-swift-without-xcode/

# A simple build script for building projects.
#
# usage: make [CONFIG=debug|release]

MODULE_NAME = yo
SDK         = macosx
ARCH        = x86_64

CONFIG     ?= debug

ROOT_DIR    = $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
OUTPUT_DIR  = $(ROOT_DIR)/bin
TARGET_DIR  = $(OUTPUT_DIR)/$(SDK)/$(CONFIG)
SRC_DIR     = $(ROOT_DIR)/yo

ifeq ($(CONFIG), debug)
	CFLAGS=-Onone -g
else
	CFLAGS=-O3
endif

SWIFTC      = $(shell xcrun -f swiftc)
CLANG       = $(shell xcrun -f clang)
SDK_PATH    = $(shell xcrun --show-sdk-path --sdk $(SDK))
SWIFT_FILES = $(shell find `pwd`/yo -type f -name '*.swift')

build:
	mkdir -p $(TARGET_DIR)
	$(SWIFTC) $(SWIFT_FILES) -emit-executable -sdk $(SDK_PATH) -o $(TARGET_DIR)/$(MODULE_NAME)

clean:
	rm -rf $(OUTPUT_DIR)
