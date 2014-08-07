#!/bin/bash
set +x

#
# Package versions
#
GO_VERSION="1.3"
NODE_VERSION="v0.10.29"         # npm=v1.4.14, shipped with node
EMACS_VERSION="dev"
THRIFT_VERSION="dev"
MONGODB_VERSION="2.6.3"


#
# DO NOT CHANGE (Assume Linux 64bit, rely on package naming convention).
#
NODE_PACKAGE="node-$NODE_VERSION-linux-x64.tar.gz"
NODE_DIR="node-$NODE_VERSION-linux-x64"
NODE_URL="http://nodejs.org/dist/$NODE_VERSION/$NODE_PACKAGE"
MONGODB_PACKAGE="mongodb-linux-x86_64-$MONGODB_VERSION.tgz"
MONGODB_DIR="mongodb-linux-x86_64-$MONGODB_VERSION"
MONGODB_URL="http://fastdl.mongodb.org/linux/$MONGODB_PACKAGE"
GO_PACKAGE="go$GO_VERSION.linux-amd64.tar.gz"
GO_DIR="go"                     # package get renamed after unzip.
GO_URL="http://golang.org/dl/$GO_PACKAGE"
THRIFT_DIR="thrift"


function InstallSystemPkg() {
  # Update packages
  sudo apt-get update
  # Basic tools
  sudo apt-get install -y git wget curl zsh mercurial build-essential \
       exfat-fuse exfat-utils
  # Build tools
  sudo apt-get install -y cmake automake libtool
  # Customization tools
  sudo apt-get install -y gnome-tweak-tool ttf-wqy-zenhei fonts-inconsolata \
       compizconfig-settings-manager compiz-plugins-extra wmctrl
  # Language tools
  sudo apt-get install -y markdown python-pip php5 php5-mysql php5-gd php5-dev \
       php5-curl php-apc php5-cli php5-json python-dev g++ libglib2.0-dev \
       libevent-dev meld lua5.2
  # Other tools
  sudo apt-get w3m
}


function InstallEmacs() {
  # Install required packages for emacs.
  sudo apt-get install -y texinfo libxpm-dev libpng-dev libgif-dev \
       libjpeg-dev libtiff-dev libgtk-3-dev libncurses5-dev
  # Build from source (emacs is a git-submodule).
  cd tools/emacs
  ./autogen.sh
  ./configure
  make
  sudo make install
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
}


function InstallNodeJs() {
  if [[ ! -e $NODE_PACKAGE ]]; then
    wget $NODE_URL
  fi
  sudo tar -C /usr/local -xvf $NODE_PACKAGE --strip 1
}


function InstallMongoDB() {
  if [[ ! -e $MONGODB_PACKAGE ]]; then
    wget $MONGODB_URL
  fi
  sudo tar -C /usr/local -xvf $MONGODB_PACKAGE --strip 1
}


function InstallThrift() {
  # Basic build requirements and language support
  sudo apt-get install -y libboost-dev libboost-test-dev flex bison pkg-config \
       libboost-program-options-dev libssl-dev
  if [[ ! -e $THRIFT_DIR ]]; then
    git clone https://git-wip-us.apache.org/repos/asf/thrift.git $THRIFT_DIR
  fi
  # Build from source.
  cd $THRIFT_DIR
  if [[ $THRIFT_VERSION != "dev" ]]; then
     git checkout tags/$THRIFT_VERSION
  fi
  ./bootstrap.sh
  ./configure
  make
  sudo make install
  cd -
}


function SetupEnvironment() {
  # Use zsh
	sudo chsh -s /usr/bin/zsh $USER
  # Intall important links
  python manager.py install
  sudo ln -sf /usr/local/bin/emacs /usr/bin/emacs
  sudo ln -sf /usr/local/bin/emacsclient /usr/bin/emacsclient
  sudo ln -sf /usr/local/bin/node /usr/bin/node
  sudo ln -sf /usr/local/bin/npm /usr/bin/npm
  sudo ln -sf /usr/local/bin/mongod /usr/bin/mongod
  # Set up z
  if [[ ! -e ~/.z ]]; then
    touch ~/.z
  fi
  # Set up git
	git config --global user.email "deyuan.deng@gmail.com"
	git config --global user.name "Deyuan Deng"
	git config --global push.default simple
  # Set up MongoDB
  RC_LOCAL=`cat /etc/init.d/rc.local`
  if [[ $RC_LOCAL != *mongod* ]]; then
    MONGODB_CMD="echo 'mongod --fork --logpath /var/log/mongodb.log --logappend'"
    sudo sh -c "$MONGODB_CMD >> /etc/init.d/rc.local"
  fi
  if [[ ! -d /data/db ]]; then
    sudo mkdir -p /data/db
  fi
  # Set up NodeJs
  npm config set tmp /tmp
  sudo npm install -g express grunt grunt-cli bower
}


function CleanUp() {
  sudo rm -rf $NODE_PACKAGE $MONGODB_PACKAGE $GO_PACKAGE $THRIFT_DIR
  cd /usr/local
  sudo rm -rf ChangeLog GNU-AGPL-3.0 LICENSE README README.md THIRD-PARTY-NOTICES
  cd -
}


######################################################################
#                         Start installation                         #
######################################################################
# #
# # Install system packages to /usr/.
# #
InstallSystemPkg

# #
# # Install packages to /usr/local/.
# #
git submodule init
git submodule update
InstallEmacs
InstallGo
InstallNodeJs
InstallMongoDB
InstallThrift

# #
# # Setup environment and clean up.
# #
SetupEnvironment
CleanUp
