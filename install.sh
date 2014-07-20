#!/bin/bash
set +x

#
# Package versions. 'dev' means build from source.
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


# Install system packages.
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
  sudo apt-get install -y markdown python-pip php5 php5-mysql php5-gd php5-dev \
       php5-curl php-apc php5-cli php5-json
}


# Install dev version of emacs to /usr/local/
function InstallEmacs() {
  echo "Installing Emacs from source..."
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


# Install Go to /usr/local/go/
function InstallGo() {
  echo "Installing GO Binary..."
  if [[ ! -e $GO_PACKAGE ]]
  then
    wget $GO_URL
  fi
  if [[ -e /usr/local/go ]]
  then
    echo "Go exists under /usr/local/go. To reinstall, press ";
    echo "RETURN, or ^C to cancel.";
    read -e ignored
    sudo rm -rf /usr/local/go
  fi
  # Go will be extracted and renamed to 'go' automatically.
  sudo tar -C /usr/local -xvf $GO_PACKAGE
}


# Install NodeJs to /usr/local/nodejs/
function InstallNodeJs() {
  echo "Installing NodeJs Binary..."
  if [[ ! -e $NODE_PACKAGE ]]
  then
    wget $NODE_URL
  fi
  if [[ -e /usr/local/nodejs ]]
  then
    echo "Node.js exists under /usr/local/nodejs. To reinstall, press ";
    echo "RETURN, or ^C to cancel.";
    read -e ignored
    sudo rm -rf /usr/local/nodejs
  fi
  sudo tar -C /usr/local -xvf $NODE_PACKAGE
  sudo mv /usr/local/$NODE_DIR /usr/local/nodejs
}


# Install MongoDB to /usr/local/mongodb/
function InstallMongoDB() {
  echo "Installing MongoDB Binary..."
  if [[ ! -e $MONGODB_PACKAGE ]]
  then
    wget $MONGODB_URL
  fi
  if [[ -e /usr/local/mongodb ]]
  then
    echo "MongoDB exists under /usr/local/mongodb. To reinstall, press ";
    echo "RETURN, or ^C to cancel.";
    read -e ignored
    sudo rm -rf /usr/local/mongodb
  fi
  sudo tar -C /usr/local -xvf $MONGODB_PACKAGE
  sudo mv /usr/local/$MONGODB_DIR /usr/local/mongodb
}


# Setup system after installation. Note PATH is updated in .zshrc.
function SetupEnvironment() {
  # Intall important links.
  python manager.py install
  if [[ ! -e ~/.z ]]
  then
    touch ~/.z
  fi
	sudo chsh -s /usr/bin/zsh $USER
	git config --global user.email "deyuan.deng@gmail.com"
	git config --global user.name "Deyuan Deng"
	git config --global push.default simple
  sudo ln -sf /usr/local/nodejs/bin/node /usr/bin/node
  sudo ln -sf /usr/local/nodejs/bin/npm /usr/bin/npm
  sudo ln -sf /usr/local/mongodb/bin/mongo /usr/bin/mongo
  sudo ln -sf /usr/local/mongodb/bin/mongod /usr/bin/mongod
  # Set up MongoDB
  RC_LOCAL=`cat /etc/init.d/rc.local`
  if [[ $RC_LOCAL != *mongod* ]]
  then
    MONGODB_CMD="echo 'mongod --fork --logpath /var/log/mongodb.log --logappend'"
    sudo sh -c "$MONGODB_CMD >> /etc/init.d/rc.local"
  fi
  if [[ ! -d /data/db ]]
  then
    sudo mkdir -p /data/db
  fi
  # Set up NodeJs
  npm config set tmp /tmp
  sudo npm install -g express grunt grunt-cli bower
}


function CleanUp() {
  rm -rf $NODE_PACKAGE $MONGODB_PACKAGE $GO_PACKAGE
}


######################################################################
#                         Start installation                         #
######################################################################
#
# Install system packages to /usr/.
#
InstallSystemPkg

#
# Install packages to /usr/local/.
#
git submodule init
git submodule update
InstallEmacs
InstallGo
InstallNodeJs
InstallMongoDB

#
# Setup environment and clean up.
#
SetupEnvironment
CleanUp
