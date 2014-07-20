#!/bin/bash
set +x


# Packages that's installed to /usr/bin.
function InstallSystemPkg() {
  sudo true
  if [[ $? -ne 0 ]]
  then
    echo "ERROR: You must be able to sudo to run this script.";
    exit 1;
  fi
  sudo apt-get update
  # Basic tools
  sudo apt-get install -y git wget curl build-essential exfat-fuse exfat-utils
  # Build tools
  sudo apt-get install -y cmake automake libtool
  # Customization tools
  sudo apt-get install -y gnome-tweak-tool ttf-wqy-zenhei fonts-inconsolata \
       compizconfig-settings-manager compiz-plugins-extra
  # Language tools
  sudo apt-get install -y markdown python-pip
}


# Configure system for installing custom packages.
function ConfigureSystem() {
  git submodule init
  git submodule update
}


# Setup system after all installation.
function SetupSystem() {
  python manager.py install
  if [[ ! -e ~/.z ]]
  then
    touch ~/.z
  fi
	sudo chsh -s /usr/bin/zsh $USER
	git config --global user.email "deyuan.deng@gmail.com"
	git config --global user.name "Deyuan Deng"
	git config --global push.default simple
}


# Install newest (dev) version of emacs to /usr/local/bin.
function InstallEmacs() {
  # Install required packages for emacs.
  sudo apt-get install -y \
       texinfo libxpm-dev libpng-dev libgif-dev libjpeg-dev libtiff-dev \
       libgtk-3-dev libncurses5-dev
  cd tools/emacs
  ./autogen.sh
  ./configure
  make
  sudo make install
  cd -
}


######################################################################
#                         Start installation                         #
######################################################################
# Install system packages to /usr/bin.
InstallSystemPkg
# Install packages to /usr/local/bin.
ConfigureSystem
#InstallEmacs
SetupSystem
