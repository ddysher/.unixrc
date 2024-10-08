# Linux configs

My unix rc and emacs config files.

## Download

Clone the repository:

```sh
git clone https://github.com/ddysher/.unixrc.git
# or
git clone git@github.com:ddysher/.unixrc.git
```

Initialize submodules:

```sh
git submodule init
git submodule update
```

Then install required symlinks:

```bash
ln -sf ~/.unixrc/.emacs.d ~/.emacs.d
ln -sf ~/.unixrc/.zshrc ~/.zshrc
ln -sf ~/.unixrc/.config/ohmyzsh/deyuan.zsh-theme ~/.unixrc/ohmyzsh/custom/themes/deyuan.zsh-theme
```

Optional symlinks:

```bash
mkdir -p ~/.config

ln -sf ~/.unixrc/.config/terminator ~/.config/terminator
ln -sf ~/.unixrc/.config/gtk-3.0 ~/.config/gtk-3.0
ln -sf ~/.unixrc/.config/.fonts.conf ~/.fonts.conf

# 'Xmodmap' change Caps to Ctrl in X11.
ln -sf ~/.unixrc/.config/.Xmodmap ~/.Xmodmap

# 'xprofile' set environment variables required by fcitx.
ln -sf ~/.unixrc/.config/.xprofile ~/.xprofile

ln -sf ~/.unixrc/.config/.npmrc ~/.npmrc
```

## Install packages

### Emacs

There are certain packages required by emacs configs:

- [w3m](http://w3m.sourceforge.net/) [required]: used as browser in emacs, install via distro's package manager.
- make, cmake, libtool [required]: used to build vterm in emacs, install via distro's package manager.
- [live markdown](https://github.com/shime/livedown) [optional]: used to view markdown file in browser while editing.
- [doctoc](https://github.com/thlorenz/doctoc) [optional]: used to generate markdown table of content.

List of commands:

```
sudo apt-get install w3m
# sudo pacman -S w3m
# brew install w3m

sudo npm install -g livedown
sudo npm install -g doctoc
```

### iTerm2

Install iTerm2 and add related profiles in "~/.unixrc/.config/iterm2", then change the keyboard binding of "option" and "command".

### Z

[z](https://github.com/rupa/z) is a productivity tool to navigate most used directories.

Create a file `~/.z` which saves z's indexes.

```sh
touch ~/.z
```

## Misc

### Update Git Submodule

To update git submodules, enter the submodule directory and pull the new changes, e.g.

```
cd tools/z
git checkout master
git pull
cd -
```

Then commit the changes and push to origin:

```
git commit -m "update git submodule z"
git push
```

In aother environment, we can pull the commit and update the submodule via:

```
git pull
git submodule update
```

### Keyboard delay/rate in X11

Keyboard delay/rate can be set in desktop environment's 'System Settings' option;
or in command line:
```sh
xset q
xset r rate 200 33
```
