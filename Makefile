all:
	@echo issue make install to install all the rc files.

install:
	git submodule init
	git submodule update
	python manager.py install

.PHONY: all clean
