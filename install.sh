#!/bin/bash
set +x


#
# Package versions
#
GO_VERSION="1.3"
NODE_VERSION="v0.10.29"       # npm version v1.4.14, shipped with node
EMACS_VERSION="dev"
THRIFT_VERSION="dev"
MONGODB_VERSION="2.6.3"
VAGRANT_VERSION="1.6.3"

#
# DO NOT CHANGE (Assume Ubuntu 64bit, rely on package naming convention).
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
VAGRANT_PACKAGE="vagrant_${VAGRANT_VERSION}_x86_64.deb"
VAGRANT_URL="https://dl.bintray.com/mitchellh/vagrant/$VAGRANT_PACKAGE"


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
  # Install required packages for emacs.
  sudo apt-get install -y texinfo libxpm-dev libpng-dev libgif-dev \
       libjpeg-dev libtiff-dev libgtk-3-dev libncurses5-dev
  # Other tools
  sudo apt-get w3m
  sudo pip install ipython
  sudo pip install pylint
}


function InstallEmacs() {
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
  if [[ ! -e thrift ]]; then
    git clone https://git-wip-us.apache.org/repos/asf/thrift.git thrift
  fi
  # Build from source.
  cd thrift
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
  sudo ln -sf /usr/local/go/bin/go /usr/bin/go
  sudo ln -sf /usr/local/go/bin/godoc /usr/bin/godoc
  sudo ln -sf /usr/local/go/bin/gofmt /usr/bin/gofmt
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
  # Clone code
  if [[ ! -d ~/code ]]; then
    cd ~
    git clone https://github.com/ddysher/code.git
    cd -
  fi
}


function InstallKubernetes() {
  export GOPATH=$HOME/code/source/go-workspace
  go get github.com/coreos/etcd
  go get github.com/tools/godep
  sudo ln -sf $GOPATH/bin/etcd /usr/bin/etcd
  sudo ln -sf $GOPATH/bin/godep /usr/bin/godep
}


function InstallVagrant() {
  wget $VAGRANT_URL
  sudo dpkg -i $VAGRANT_PACKAGE
}


function InstallDocker() {
  # Only works for > 14.04
  sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 36A1D7869245C8950F966E92D8576A8BA88D21E9
  sudo sh -c "echo deb https://get.docker.io/ubuntu docker main > /etc/apt/sources.list.d/docker.list"
  sudo apt-get update
  sudo apt-get install lxc-docker
}


function CleanUp() {
  sudo rm -rf $NODE_PACKAGE $MONGODB_PACKAGE $GO_PACKAGE $VAGRANT_PACKAGE
  sudo rm -rf thrift
  cd /usr/local
  sudo rm -rf ChangeLog GNU-AGPL-3.0 LICENSE README README.md THIRD-PARTY-NOTICES
  cd -
}


######################################################################
#                         Start installation                         #
######################################################################
if [[ $USER = "root" ]]; then
   echo "Do not run as root."
   exit
fi
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
InstallThrift
InstallVagrant
InstallKubernetes
InstallDocker

#
# Setup environment and clean up.
#
SetupEnvironment
CleanUp
