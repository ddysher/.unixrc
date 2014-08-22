#!/bin/bash
set +x

#
# Mac version
#
MAC_VERSION=10.8

#
# Package versions
# Package versions do not matter that much in Mac.
#
GO_VERSION="1.3"


#
# DO NOT CHANGE (Assume Ubuntu 64bit, rely on package naming convention).
#
GO_PACKAGE="go${GO_VERSION}.darwin-amd64-osx${MAC_VERSION}.tar.gz"
GO_DIR="go"                     # package gets renamed after unzip.
GO_URL="http://golang.org/dl/$GO_PACKAGE"


#
# Entry point
# NOTE: the installation order matters.
#
function InstallAll() {

  if [[ $USER = "root" ]]; then
    echo "Do not run as root, configuration depends on user name."
    exit
  fi

  # Install system packages to /usr/.
  InstallSystemPkg

  # # Install packages to /usr/local/.
  git submodule init
  git submodule update
  InstallThirdPartyPkg
  InstallEmacs
  InstallGo

  # # Setup environment and clean up.
  SetupEnvironment
  CleanUp
}


function InstallSystemPkg() {
  # Install and/or update homebrew
  echo "Installing homebrew..."
  ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
  echo "Updating homebrew..."
  brew update

  # Basic packages
  echo "Installing packages..."
  sudo brew install wget tree w3m pkg-config automake autoconf
  sudo brew install mercurial node mongodb

  # Resolve any conflict
  sudo brew link --overwrite node
}


function InstallThirdPartyPkg() {
  # Newer version of python, also install pip. But unless necessary, prefer
  # using default python.
  #   sudo brew install python
  sudo easy_install pip
  sudo pip install ipython --upgrade
  sudo pip install pylint --upgrade
}


function InstallEmacs() {
  cd tools/emacs
  ./autogen.sh
  ./configure --with-ns
  make
  sudo make install
  yes | cp -r nextstep/Emacs.app /Applications
  cd -
}


function InstallGo() {
  if [[ ! -e $GO_PACKAGE ]]; then
    wget $GO_URL
  fi
  # Keep it simple, force delete.
  sudo rm -rf /usr/local/go
  # Install Go to /usr/local/go/.
  sudo tar -C /usr/local -xvf $GO_PACKAGE
  # Go tools
  export GOPATH=$HOME/code/source/go-workspace
  go get github.com/tools/godep
  go get github.com/nsf/gocode
  go get code.google.com/p/rog-go/exp/cmd/godef
  go get code.google.com/p/go.tools/cmd/goimports
}


function SetupEnvironment() {
  # Use zsh
  sudo chsh -s /usr/bin/zsh $USER
  # Intall important links
  rm -rf ~/.emacs.d ~/.zshrc	# Force delete first
  sudo ln -sf ~/.unixrc/.emacs.d ~/.emacs.d
  sudo ln -sf ~/.unixrc/.zshrc ~/.zshrc
  sudo ln -sf /usr/local/go/bin/go /usr/bin/go
  # Set up z
  if [[ ! -e ~/.z ]]; then
    touch ~/.z
  fi
  # Set up git
  git config --global user.email "deyuan.deng@gmail.com"
  git config --global user.name "Deyuan Deng"
  git config --global push.default simple
  if [[ ! -d /data/db ]]; then
    sudo mkdir -p /data/db
  fi
  # Set up NodeJs
  npm config set tmp /tmp
  sudo npm install -g express grunt grunt-cli bower
  # Clone code
  if [[ ! -d ~/code ]]; then
    cd ~
    git clone https://github.com/ddysher/code.git
    cd -
  fi
}


function CleanUp() {
  sudo rm -rf $GO_PACKAGE
}


InstallAll
