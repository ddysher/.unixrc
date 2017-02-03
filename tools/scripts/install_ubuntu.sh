#!/bin/bash
set +x

# Installation script for ubuntu14.04 local host.

#
# Package versions
#
DOCKER_VERSION="1.7.0"
EMACS_VERSION="24.5"
ETCD_VERSION="v2.0.13"
GLOBAL_VERSION="6.4"
GO_VERSION="1.5.1"
MONGODB_VERSION="3.0.4"
NODE_VERSION="v0.12.5"
PROXYCHAINS_VERSION="4.10"
VAGRANT_VERSION="1.7.2"
VIRTUALBOX_VERSION="4.3"

#
# DO NOT CHANGE (Assume Ubuntu 64bit, rely on package naming convention).
#
EMACS_PACKAGE="emacs-${EMACS_VERSION}.tar.xz"
EMACS_DIR="emacs-${EMACS_VERSION}"
EMACS_URL="http://gnu.mirror.constant.com/emacs/emacs-${EMACS_VERSION}.tar.xz"
ETCD_URL="https://github.com/coreos/etcd/releases/download/${ETCD_VERSION}/etcd-${ETCD_VERSION}-linux-amd64.tar.gz"
ETCD_PACKAGE="etcd-${ETCD_VERSION}-linux-amd64.tar.gz"
ETCD_DIR="etcd-${ETCD_VERSION}-linux-amd64"
GLOBAL_PACKAGE="global-${GLOBAL_VERSION}.tar.gz"
GLOBAL_DIR="global-${GLOBAL_VERSION}"
GLOBAL_URI="http://tamacom.com/global/${GLOBAL_PACKAGE}"
GO_PACKAGE="go$GO_VERSION.linux-amd64.tar.gz"
GO_DIR="go"                     # Package gets renamed after unzip.
GO_URL="http://golang.org/dl/$GO_PACKAGE"
MONGODB_PACKAGE="mongodb-linux-x86_64-$MONGODB_VERSION.tgz"
MONGODB_DIR="mongodb-linux-x86_64-$MONGODB_VERSION"
MONGODB_URL="http://fastdl.mongodb.org/linux/$MONGODB_PACKAGE"
NODE_PACKAGE="node-$NODE_VERSION-linux-x64.tar.gz"
NODE_DIR="node-$NODE_VERSION-linux-x64"
NODE_URL="http://nodejs.org/dist/$NODE_VERSION/$NODE_PACKAGE"
PROXYCHAINS_URL="https://github.com/rofl0r/proxychains-ng/archive/v${PROXYCHAINS_VERSION}.tar.gz"
PROXYCHAINS_PACKAGE="v${PROXYCHAINS_VERSION}.tar.gz"
PROXYCHAINS_DIR="proxychains-ng-${PROXYCHAINS_VERSION}"
VAGRANT_PACKAGE="vagrant_${VAGRANT_VERSION}_x86_64.deb"
VAGRANT_URL="https://dl.bintray.com/mitchellh/vagrant/${VAGRANT_PACKAGE}"


#
# Entry point
#
function InstallAll() {
  if [[ $USER = "root" ]]; then
    echo "Do not run as root, configuration depends on user name."
    exit
  fi

  InstallBasicPackages
  InstallCustomizationPackages

  git submodule init
  git submodule update

  InstallDocker
  InstallEmacs
  InstallEtcd
  InstallGo
  InstallMongoDB
  InstallNodeJs
  InstallOwncloud
  InstallRuby
  InstallVagrant
  InstallVirtualbox

  SetupEnvironment
}


# Install basic packages directly from system or thirdparty managed repositories.
function InstallBasicPackages() {
  # Install basic tools
  sudo apt-get update
  sudo apt-get install -y \
       wget curl zsh mercurial build-essential exfat-fuse exfat-utils \
       cmake automake libtool meld terminator wmctrl ttf-wqy-zenhei \
       fonts-inconsolata
  # Install language tools
  sudo apt-get install -y \
       markdown python-pip python-dev g++ libglib2.0-dev libevent-dev
  sudo pip install ipython --upgrade
  sudo pip install pylint --upgrade
  sudo pip install virtualenv --upgrade
  # Install latest git.
  sudo apt-get autoremove -y git
  sudo add-apt-repository -y ppa:git-core/ppa
  sudo apt-get update
  sudo apt-get install -y git
}


# Install packages used to customize ubuntu (14.04).
function InstallCustomizationPackages() {
  sudo apt-get install -y \
       gnome-tweak-tool compizconfig-settings-manager compiz-plugins-extra
}


# Only works for ubuntu >= 14.04
function InstallDocker() {
  sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 36A1D7869245C8950F966E92D8576A8BA88D21E9
  sudo sh -c "echo deb https://get.docker.io/ubuntu docker main > /etc/apt/sources.list.d/docker.list"
  sudo apt-get update
  sudo apt-get install -y lxc-docker-${DOCKER_VERSION}
  # Give current user non-root access, need restart machine for this to work.
  sudo usermod -a -G docker $USER
}


# Installs emacs from source code, and other plugins.
function InstallEmacs() {
  # Install required package for building emacs and plugins.
  sudo apt-get update
  sudo apt-get install -y \
       texinfo libxpm-dev libpng-dev libgif-dev libjpeg-dev libtiff-dev \
       libgtk-3-dev libncurses5-dev w3m python-pip

  # Install stock emacs.
  if [[ ! -e ${EMACS_PACKAGE} ]]; then
    wget ${EMACS_URL}
  fi
  tar -xvf ${EMACS_PACKAGE}
  cd ${EMACS_DIR}
  ./autogen.sh
  ./configure
  make
  sudo make install
  cd -
  sudo ln -sf /usr/local/bin/emacs /usr/bin/emacs
  sudo ln -sf /usr/local/bin/emacs /usr/bin/emacs
  sudo ln -sf /usr/local/bin/emacsclient /usr/bin/emacsclient
  rm -rf ${EMACS_PACKAGE}
  rm -rf ${EMACS_DIR}

  # Install global, a taging system for emacs.
  if [[ ! -e ${GLOBAL_PACKAGE} ]]; then
    wget ${GLOBAL_URI}
  fi
  tar -xvf ${GLOBAL_PACKAGE}
  cd ${GLOBAL_DIR}
  ./configure
  make
  sudo make install
  cd -
  rm -rf ${GLOBAL_PACKAGE}
  rm -rf ${GLOBAL_DIR}

  # For emacs jedi plugin
  sudo pip install jedi --upgrade
  sudo pip install epc --upgrade
}


# Install etcd binaries directly (copy to /usr/local/bin).
function InstallEtcd() {
  if [[ ! -e ${ETCD_PACKAGE} ]]; then
    wget ${ETCD_URL}
  fi
  tar xzvf ${ETCD_PACKAGE}
  sudo cp ${ETCD_DIR}/etcd ${ETCD_DIR}/etcdctl /usr/local/bin
  sudo ln -sf /usr/local/bin/etcd /usr/bin/etcd
  rm -rf ${ETCD_PACKAGE} ${ETCD_DIR}
}


# Install go binaries directly (copy to /usr/local/go).
function InstallGo() {
  if [[ ! -e $GO_PACKAGE ]]; then
    wget $GO_URL
  fi
  # Keep it simple, force delete.
  sudo rm -rf /usr/local/go
  # Install Go to /usr/local/go/.
  sudo tar -C /usr/local -xvf $GO_PACKAGE
  # Make go available to root.
  sudo ln -sf /usr/local/go/bin/go /usr/bin/go
  # Make sure we have GOPATH.
  if [[ ! -d ~/code ]]; then
    cd ~
    git clone https://github.com/ddysher/code.git
    cd -
  fi
  export GOPATH=$HOME/code/source/go-workspace
  # Instll Go tools.
  go get github.com/nsf/gocode
  go get github.com/tools/godep
  go get github.com/rogpeppe/godef
  go get golang.org/x/tools/cmd/goimports
  rm -rf $GO_PACKAGE
}

# Install mongodb binaries directly (copy to /usr/local/bin). All mongodb
# binaries start with mongo*. Reinstall will overwrite them.
function InstallMongoDB() {
  if [[ ! -e $MONGODB_PACKAGE ]]; then
    wget $MONGODB_URL
  fi
  # MongoDB has one more level of directory.
  sudo tar -C /usr/local -xvf $MONGODB_PACKAGE --strip 1
  # Make mongod available to root.
  sudo ln -sf /usr/local/bin/mongod /usr/bin/mongod
  # Create data directory.
  if [[ ! -d /data/db ]]; then
    sudo mkdir -p /data/db
  fi
  rm -rf $MONGODB_PACKAGE
  # Set up MongoDB to start on boot. TODO: A better solution for startup daemon.
  if false; then
    RC_LOCAL=`cat /etc/init.d/rc.local`
    if [[ $RC_LOCAL != *mongod* ]]; then
      MONGODB_CMD="echo 'mongod --fork --logpath /var/log/mongodb.log --logappend'"
      sudo sh -c "$MONGODB_CMD >> /etc/init.d/rc.local"
    fi
  fi
}


# Install nodejs binaries directly (copy to /usr/local). Uninstall is hard.
# Reinstall will overwrite old version.
function InstallNodeJs() {
  if [[ ! -e $NODE_PACKAGE ]]; then
    wget $NODE_URL
  fi
  sudo tar -C /usr/local -xvf $NODE_PACKAGE --strip 1
  # Make nodejs available to root.
  sudo ln -sf /usr/local/bin/node /usr/bin/node
  sudo ln -sf /usr/local/bin/npm /usr/bin/npm
  npm config set tmp /tmp
  # Install common packages used with nodejs
  sudo npm install -g express grunt grunt-cli bower
  rm -rf $NODE_PACKAGE
  cd /usr/local
  sudo rm -rf ChangeLog GNU-AGPL-3.0 LICENSE README README.md THIRD-PARTY-NOTICES
  cd -
}


# Install owncloud client, for ubuntu 14.04
function InstallOwncloud() {
  wget http://download.opensuse.org/repositories/isv:ownCloud:desktop/xUbuntu_14.04/Release.key
  sudo apt-key add - < Release.key
  rm -rf Release.key
  sudo sh -c "echo 'deb http://download.opensuse.org/repositories/isv:/ownCloud:/desktop/xUbuntu_14.04/ /' > /etc/apt/sources.list.d/owncloud-client.list"
  sudo apt-get update
  sudo apt-get install -y owncloud-client
}


# Install ruby - just clone rbenv, and set PATH in .zshrc.
function InstallRuby() {
  git clone https://github.com/sstephenson/rbenv.git ~/.rbenv
  git clone https://github.com/sstephenson/ruby-build.git ~/.rbenv/plugins/ruby-build
}


# Install and setup shadowsocks for ubuntu.
# After installation, start local proxy via:
#   $ sslocal -s $server-ip -p 8388 -b 127.0.0.1 -l 1080 -k $password -t 600 -m aes-256-cfb
# This will start a local proxy running on 127.0.0.1:1080.
#
# To use the proxy on command line, run:
#   $ proxychains4 zsh
# This will bring up a new zsh session where all traffic will be proxied. Note
# however, commands using sudo will not be proxied since it will run in a new
# sudo context; therefore, we need to use sudo and proxychains4 together, e.g.:
#   $ sudo proxychains4 apt-get update
function InstallShadowsocks() {
  sudo apt-get update
  sudo apt-get install -y python-pip
  sudo pip install shadowsocks

  # Install proxychains-ng
  if [[ ! -e ${PROXYCHAINS_PACKAGE} ]]; then
    wget ${PROXYCHAINS_URL}
  fi
  tar -xvf ${PROXYCHAINS_PACKAGE}
  cd ${PROXYCHAINS_DIR}
  ./configure --prefix=/usr --sysconfdir=/etc
  sudo make install
  cd -
  rm -rf ${PROXYCHAINS_DIR} ${PROXYCHAINS_PACKAGE}

  mkdir -p ~/.proxychains
  sudo cat <<EOF > ~/.proxychains/proxychains.conf
strict_chain
proxy_dns
remote_dns_subnet 224
tcp_read_time_out 15000
tcp_connect_time_out 8000
localnet 127.0.0.0/255.0.0.0
quiet_mode

[ProxyList]
socks5  127.0.0.1 1080
EOF
}


# Install vagrant using dpkg.
function InstallVagrant() {
  if [[ ! -e $VAGRANT_PACKAGE ]]; then
    wget $VAGRANT_URL
  fi
  sudo dpkg -i $VAGRANT_PACKAGE
  rm -rf $VAGRANT_PACKAGE
}


# Install virtualbox.
function InstallVirtualbox() {
  sudo sh -c "echo deb http://download.virtualbox.org/virtualbox/debian trusty contrib > /etc/apt/sources.list.d/virtualbox.list"
  wget -q https://www.virtualbox.org/download/oracle_vbox.asc -O- | sudo apt-key add -
  rm -rf oracle_vbox.asc
  sudo apt-get update
  sudo apt-get install -y virtualbox-${VIRTUALBOX_VERSION}
}


function SetupEnvironment() {
  # Use zsh
  sudo apt-get install -y zsh
  sudo chsh -s /usr/bin/zsh $USER
  # Intall links
  ln -sf ~/.unixrc/.emacs.d ~/.emacs.d
  ln -sf ~/.unixrc/.zshrc ~/.zshrc
  mkdir -p ~/.config
  ln -sf ~/.unixrc/.config/terminator ~/.config/terminator
  ln -sf ~/.unixrc/.Xmodmap ~/.Xmodmap
  ln -sf ~/.unixrc/.xprofile ~/.xprofile
  # Set up z
  if [[ ! -e ~/.z ]]; then
    touch ~/.z
  fi
  # Set up git
  git config --global user.email "deyuan.deng@gmail.com"
  git config --global user.name "Deyuan Deng"
  git config --global push.default simple
  # Clone code
  if [[ ! -d ~/code ]]; then
    cd ~
    git clone https://github.com/ddysher/code.git
    cd -
  fi
}
