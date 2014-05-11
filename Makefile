#
# A simple Makefile to install working environment on Linux (or Mac?).
#
# As a step by step work, currently only used for ubuntu.
#

OS=$(lsb_release -si)
ARCH=$(uname -m | sed 's/x86_//;s/i[3-6]86/32/')
VER=$(lsb_release -sr)


all:
	@echo Issue make install to install all the rc files.


basic:
	apt-get update
	apt-get install -y git
	apt-get install -y zsh
	apt-get install -y build-essential
	chsh -s /bin/zsh $$USER


emacs:
	apt-get install -y texinfo
	apt-get install -y libtool
	apt-get install -y automake
	apt-get install -y libxpm-dev
	apt-get install -y libpng-dev
	apt-get install -y libgif-dev
	apt-get install -y libjpeg-dev
	apt-get install -y libtiff-dev
	apt-get install -y libgtk-3-dev
	apt-get install -y libncurses5-dev
	apt-get install -y w3m w3m-img # For w3m mode, not required for building
	cd emacs && ./autogen.sh && ./configure
	make -C emacs 
	make -C emacs install


# Install new unix environment includes:
# 1. Update submodule, e.g. emacs, zsh, etc.
# 2. Install symlink and packages using python scripts.
install: basic emacs
	git submodule init
	git submodule update
	cd scripts && python manager.py install


.PHONY: all clean basic emacs
