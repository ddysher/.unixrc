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
	PKG_TOOL=apt-get install
	EMACS_CONFIG=""
endif
ifeq ($(OS), Darwin)
	DEPS_FILE="scripts/darwin_deps"
	PKG_TOOL=
	EMACS_CONFIG="--with-ns"
endif
DEPS= $(shell grep -v "^\#" $(DEPS_FILE))


# Rules
all:
	@echo Issue make install to install all the rc files.


deps:
	git submodule init
	git submodule update
	$(foreach var, $(DEPS), $(PKG_TOOL) $(var);)

emacs:
	cd emacs && ./autogen.sh && ./configure $(EMACS_CONFIG)
	make -C emacs 
	make -C emacs install



# Install new unix environment includes:
# 1. Update submodule, e.g. emacs, zsh, etc.
# 2. Install symlink and packages using python scripts.
install: deps emacs
	cd scripts && python manager.py install
	[ -f ~/.z ] || touch ~/.z
	chsh -s /usr/bin/zsh $$USER
	@echo "--------------------"
	@echo "  Done :)"
	@echo "--------------------"


.PHONY: all basic emacs clean
