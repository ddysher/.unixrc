# Unixrc

Personal Unix shell, editor, and terminal configuration.

## Bootstrap

```sh
git clone https://github.com/ddysher/.unixrc.git ~/.unixrc
# or
git clone git@github.com:ddysher/.unixrc.git ~/.unixrc

# for creating symlinks
mkdir -p ~/.config
```

### macOS

The macOS config assumes Homebrew under `/opt/homebrew`.

Install the core packages used below:

```sh
brew install zoxide starship cmake libtool
```

Set Zsh as the default shell if needed:

```sh
chsh -s "$(command -v zsh)"
```

### Linux

Install the packages used below using your distro package manager, e.g.

```sh
sudo apt-get install cmake automake libtool
```

Create Linux-only desktop/X11 symlinks:

```sh
mkdir -p ~/.config

ln -sfn ~/.unixrc/config/linux/gtk-3.0 ~/.config/gtk-3.0
ln -sfn ~/.unixrc/config/linux/fonts.conf ~/.fonts.conf
ln -sfn ~/.unixrc/config/linux/Xmodmap ~/.Xmodmap
ln -sfn ~/.unixrc/config/linux/xprofile ~/.xprofile
ln -sfn ~/.unixrc/config/linux/autokey ~/.config/autokey
```

Notes:

- [`config/linux/Xmodmap`](config/linux/Xmodmap) changes Caps Lock to Ctrl in X11.
- [`config/linux/xprofile`](config/linux/xprofile) contains X11 session environment setup.
- [`config/linux/autokey`](config/linux/autokey) stores Linux AutoKey bindings.

## Terminal Setup

### Ghostty

Ghostty is the active terminal config in this repo.

```sh
mkdir -p ~/.config
ln -sfn ~/.unixrc/config/ghostty ~/.config/ghostty
```

The config lives in [`config/ghostty`](config/ghostty).

### Shell

The shell setup is centered on [`zshrc.d/zshrc`](zshrc.d/zshrc) and enables tools when they are available locally.

```sh
ln -sfn ~/.unixrc/zshrc.d/zshrc ~/.zshrc
```

Install `zinit` directly into the user data directory:

```sh
mkdir -p ~/.local/share/zinit
git clone --depth=1 https://github.com/zdharma-continuum/zinit.git \
  ~/.local/share/zinit/zinit.git
```

Zsh tools used by this repo:

- `zsh` as the primary shell, loaded from [`zshrc.d/zshrc`](zshrc.d/zshrc)
- `zinit` as the Zsh plugin manager
- `zsh-autosuggestions` and `zsh-syntax-highlighting` via Zinit
- `zoxide` for directory jumping
- `starship` for the shell prompt
- `kubectl` completion, cached by the Zsh completion module when `kubectl` is installed
- optional local overrides via `zshrc.d/local.zsh`

Archived terminal and shell configs live under [`config/archive`](config/archive).

## Emacs Setup

The Emacs config lives in [`emacs.d`](emacs.d) and is loaded via the `~/.emacs.d` symlink.

```sh
ln -sfn ~/.unixrc/emacs.d ~/.emacs.d
```

Some Emacs features expect extra tools:

- `make`, `cmake`, and `libtool` for building native packages such as `vterm`
- `livedown` optionally for Markdown preview
- `doctoc` optionally for Markdown table of contents generation

Optional npm packages:

```sh
npm install -g livedown doctoc
```

## Development Setup

### Node.js

- `fnm` is used to manage Node.js versions.
- [`zshrc.d/languages/node.zsh`](zshrc.d/languages/node.zsh) sets `NPM_CONFIG_REGISTRY` to `https://registry.npmmirror.com/`.

### Python

- `uv` is the Python toolchain expected by the shell config.
- [`zshrc.d/languages/python.zsh`](zshrc.d/languages/python.zsh) sets `PIP_INDEX_URL` to the Tsinghua PyPI mirror.

### Other Development Tools

- `rbenv` is initialized when present locally.
- Go environment defaults live in [`zshrc.d/languages/golang.zsh`](zshrc.d/languages/golang.zsh).
- Google Cloud SDK, Azure CLI, and CUDA hooks live under [`zshrc.d/tools`](zshrc.d/tools).
