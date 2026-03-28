# Unixrc

Personal Unix shell, editor, and terminal configuration.

This repo is used on both macOS and Linux. The Zsh entrypoint auto-loads
platform-specific settings from [`zshrc.d/platform/macos.zsh`](/Users/deyuan/.unixrc/zshrc.d/platform/macos.zsh)
or [`zshrc.d/platform/linux.zsh`](/Users/deyuan/.unixrc/zshrc.d/platform/linux.zsh).

Ghostty is the current terminal target. iTerm2 config is still kept in the repo
as a fallback during migration.

## Clone

```sh
git clone --recurse-submodules https://github.com/ddysher/.unixrc.git ~/.unixrc
# or
git clone --recurse-submodules git@github.com:ddysher/.unixrc.git ~/.unixrc
```

If the repo was cloned without submodules:

```sh
cd ~/.unixrc
git submodule update --init --recursive
```

## Base Setup

Create the core symlinks:

```sh
ln -sfn ~/.unixrc/emacs.d ~/.emacs.d
ln -sfn ~/.unixrc/zshrc.d/zshrc ~/.zshrc
ln -sfn ~/.unixrc/zshrc.d/ohmyzsh/deyuan.zsh-theme \
  ~/.unixrc/tools/ohmyzsh/custom/themes/deyuan.zsh-theme
```

Optional local overrides:

```sh
cp ~/.unixrc/zshrc.d/local.zsh.example ~/.unixrc/zshrc.d/local.zsh
```

Optional but recommended for `z.sh`:

```sh
touch ~/.z
```

Set Zsh as the default shell if needed:

```sh
chsh -s "$(command -v zsh)"
```

## Platform Setup

### macOS

The macOS config assumes Homebrew under `/opt/homebrew`.

Install commonly used dependencies:

```sh
brew install zsh w3m cmake libtool fnm
```

### Linux

Install the common packages required by this repo's shell and Emacs setup using
your distro package manager. Typical packages include:

- `zsh`
- `w3m`
- `cmake`
- `automake`
- `libtool`

Linux-only desktop/X11 symlinks:

```sh
mkdir -p ~/.config

ln -sfn ~/.unixrc/config/terminator ~/.config/terminator
ln -sfn ~/.unixrc/config/gtk-3.0 ~/.config/gtk-3.0
ln -sfn ~/.unixrc/config/fonts.conf ~/.fonts.conf
ln -sfn ~/.unixrc/config/Xmodmap ~/.Xmodmap
ln -sfn ~/.unixrc/config/xprofile ~/.xprofile
```

Notes:

- [`config/Xmodmap`](/Users/deyuan/.unixrc/config/Xmodmap) changes Caps Lock to Ctrl in X11.
- [`config/xprofile`](/Users/deyuan/.unixrc/config/xprofile) contains X11 session environment setup.

## Terminal Setup

### Ghostty

Ghostty is the preferred terminal now.

Create the config symlink:

```sh
mkdir -p ~/.config/ghostty
ln -sfn ~/.unixrc/config/ghostty/config ~/.config/ghostty/config
```

Current Ghostty config lives in [`config/ghostty/config`](/Users/deyuan/.unixrc/config/ghostty/config).

### iTerm2

iTerm2 is still supported as a migration fallback on macOS. Related files are in
[`config/iterm2`](/Users/deyuan/.unixrc/config/iterm2).

## Other Optional Config

```sh
ln -sfn ~/.unixrc/config/npm/.npmrc ~/.npmrc
```

Fish config is also stored in [`config/fish`](/Users/deyuan/.unixrc/config/fish),
but Zsh is the primary shell in this repo.

## Emacs Notes

Some Emacs features expect external tools:

- `w3m` for in-Emacs browsing
- `make`, `cmake`, and `libtool` for building native packages such as `vterm`
- `livedown` optionally for Markdown preview
- `doctoc` optionally for Markdown table of contents generation

Optional npm packages:

```sh
npm install -g livedown doctoc
```

## Submodules

To update bundled submodules:

```sh
git submodule update --init --recursive --remote
```
