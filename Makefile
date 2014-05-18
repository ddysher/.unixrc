#
# A simple Makefile to install working environment on Linux (or Mac?).
#
# As a step by step work, currently only used for ubuntu.
#
# DISTRO=$(lsb_release -si)
# ARCH=$(uname -m | sed 's/x86_//;s/i[3-6]86/32/')
# VER=$(lsb_release -sr)


# Platform related variables
OS=$(shell uname)
ifeq ($(OS), Linux)
	DEPS_FILE="scripts/linux_deps"
	PKG_TOOL=apt-get install -y
	EMACS_CONFIG=""
endif
ifeq ($(OS), Darwin)
	DEPS_FILE="scripts/darwin_deps"
	PKG_TOOL=brew install
	EMACS_CONFIG="--with-ns"
endif
DEPS= $(shell grep -v "^\#" $(DEPS_FILE))


# Rules (TODO: curl and whoami)
deps:
	git submodule init
	git submodule update
ifeq ($(OS), Darwin)
	ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
	chown root $(shell which brew)
endif
	$(foreach var, $(DEPS), $(PKG_TOOL) $(var);)
ifeq ($(OS), Darwin)
	chown $(shell whoami) $(shell which brew)
endif


emacs:
	cd emacs && ./autogen.sh && ./configure $(EMACS_CONFIG)
	make -C emacs 
	make -C emacs install

# Install new Unix environment includes:
# 1. Update submodule, e.g. emacs, zsh, etc.
# 2. Install submodules and other packages.
# 2. Install symlink using python scripts.
install: deps emacs
	cd scripts && python manager.py install
	[ -f ~/.z ] || touch ~/.z
	chsh -s /usr/bin/zsh $$USER
	@echo "--------------------"
	@echo "  Done :)"
	@echo "--------------------"


.PHONY: all clean basic emacs
