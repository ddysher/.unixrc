all:
	@echo Issue 'make install' to install all the rc files.


basic:
	apt-get install git

# Install new unix environment includes:
# 1. Update submodule, e.g. emacs, zsh, etc.
# 2. Install symlink and packages using python scripts.
install: basic
	git submodule init
	git submodule update
	cd scripts && python manager.py install

.PHONY: all clean
