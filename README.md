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

Additionally, clone [code](https://github.com/ddysher/code) repository.

## Install packages

TODO

## Install dependencies

### Emacs

There are certain packages required by emacs configs:

- w3m [required], used as browser in emacs, install `w3m` from distro's package
  manager will suffice, e.g. `sudo pacman -S w3m`.

- live markdown [optional], used to view markdown file in browser while editing
  (bound to Ctrl-Alt-M). See https://github.com/shime/livedown.

### Golang

A bunch of golang tools need to be installed to make both go and emacs work
properly (GOPATH is set to ~/code/workspace in .zshrc; so make sure to run
the commands in a new zsh shell).

```sh
go get github.com/nsf/gocode
go get github.com/tools/godep
go get github.com/rogpeppe/godef
go get golang.org/x/tools/cmd/guru
go get golang.org/x/tools/cmd/goimports
```

### Z

`z` is for directory jump, see https://github.com/rupa/z. If will complain if
`~/.z` doesn't exist, so touch it anyway:

```sh
touch ~/.z
```

## Install links

```bash
mkdir -p ~/.config

ln -sf ~/.unixrc/.emacs.d ~/.emacs.d
ln -sf ~/.unixrc/.zshrc ~/.zshrc

ln -sf ~/.unixrc/.config/terminator ~/.config/terminator
ln -sf ~/.unixrc/.config/gtk-3.0 ~/.config/gtk-3.0
ln -sf ~/.unixrc/.config/.fonts.conf ~/.fonts.conf

# 'Xmodmap' change Caps to Ctrl in X11.
ln -sf ~/.unixrc/.config/.Xmodmap ~/.Xmodmap

# 'xprofile' set environment variables required by fcitx.
ln -sf ~/.unixrc/.config/.xprofile ~/.xprofile

ln -sf ~/.unixrc/.config/.npmrc ~/.npmrc
```

## Misc

### Keyboard delay/rate in X11

Keyboard delay/rate can be set in desktop environment's 'System Settings' option;
or in command line:
```sh
xset q
xset r rate 200 33
```
