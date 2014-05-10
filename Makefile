all:
	@echo Issue 'make install' to install all the rc files.

install:
	git submodule init
	git submodule update
	cd scripts && python manager.py install

.PHONY: all clean
