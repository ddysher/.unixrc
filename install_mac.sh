#!/bin/bash
set +x


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

  # Install packages to /usr/local/.
  git submodule init
  git submodule update
  InstallThirdPartyPkg
  InstallEmacs
  InstallGo

  # Setup environment and clean up.
  SetupEnvironment
  CleanUp
}


function InstallSystemPkg() {
  # Install homebrew
  ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
}


function InstallThirdPartyPkg() {
  sudo pip install ipython --upgrade
  sudo pip install pylint --upgrade
}


function InstallEmacs() {
  cd tools/emacs
  ./autogen.sh
  ./configure --with-ns
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


function SetupEnvironment() {
  # Use zsh
	sudo chsh -s /usr/bin/zsh $USER
  # Intall important links
  sudo ln -sf ~/.unixrc/.emacs.d ~/.emacs.d
  sudo ln -sf ~/.unixrc/.zshrc ~/.zshrc
  sudo ln -sf /usr/local/bin/emacs /usr/bin/emacs
  sudo ln -sf /usr/local/bin/emacs /usr/bin/emacs
  sudo ln -sf /usr/local/bin/emacsclient /usr/bin/emacsclient
  sudo ln -sf /usr/local/bin/node /usr/bin/node
  sudo ln -sf /usr/local/bin/npm /usr/bin/npm
  sudo ln -sf /usr/local/bin/mongod /usr/bin/mongod
  sudo ln -sf /usr/local/go/bin/go /usr/bin/go
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


function CleanUp() {
  sudo rm -rf $NODE_PACKAGE $MONGODB_PACKAGE $GO_PACKAGE $VAGRANT_PACKAGE
  cd /usr/local
  sudo rm -rf ChangeLog GNU-AGPL-3.0 LICENSE README README.md THIRD-PARTY-NOTICES
  cd -
}


InstallAll
