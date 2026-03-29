# Unixrc

Personal Unix shell, editor, and terminal configuration.

## Clone

```sh
git clone https://github.com/ddysher/.unixrc.git ~/.unixrc
# or
git clone git@github.com:ddysher/.unixrc.git ~/.unixrc
```

## Base Setup

Create the core symlinks:

```sh
ln -sfn ~/.unixrc/emacs.d ~/.emacs.d
ln -sfn ~/.unixrc/zshrc.d/zshrc ~/.zshrc
```

Optional local overrides:

```sh
cp ~/.unixrc/zshrc.d/local.zsh.example ~/.unixrc/zshrc.d/local.zsh
```

Install `zinit` directly into the user data directory:

```sh
mkdir -p ~/.local/share/zinit
git clone --depth=1 https://github.com/zdharma-continuum/zinit.git \
  ~/.local/share/zinit/zinit.git
```

Install `zoxide` on macOS:

```sh
brew install zoxide
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
brew install w3m cmake libtool
```

### Linux

Install the common packages required by this repo's shell and Emacs setup using
your distro package manager. Typical packages include:

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

- [`config/Xmodmap`](config/Xmodmap) changes Caps Lock to Ctrl in X11.
- [`config/xprofile`](config/xprofile) contains X11 session environment setup.

## Emacs Setup

Some Emacs features expect external tools:

- `w3m` for in-Emacs browsing
- `make`, `cmake`, and `libtool` for building native packages such as `vterm`
- `livedown` optionally for Markdown preview
- `doctoc` optionally for Markdown table of contents generation

Optional npm packages:

```sh
npm install -g livedown doctoc
```

## Terminal Setup

### Ghostty

Ghostty is the preferred terminal now.

Create the config symlink:

```sh
ln -sfn ~/.unixrc/config/ghostty ~/.config/ghostty
```

Current Ghostty config lives in [`config/ghostty`](config/ghostty).

### iTerm2

iTerm2 is still supported as a migration fallback on macOS. Related files are in [`config/iterm2`](config/iterm2).

## Development Setup

### Node.js

```sh
ln -sfn ~/.unixrc/config/npm/.npmrc ~/.npmrc
```

- `fnm` is used to manage node versions, installed or upgraded via the official curl script.

## Shell Dependencies

External shell dependencies:

- `zinit` installed at `~/.local/share/zinit/zinit.git`
- `zoxide` installed via Homebrew on macOS

## Others

Fish config is also stored in [`config/fish`](config/fish),
but Zsh is the primary shell in this repo.
