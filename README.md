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
- optional local overrides via [`zshrc.d/local.zsh.example`](zshrc.d/local.zsh.example)

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

#### Python (Emacs)

Python support uses `eglot` + `pyright` for LSP and `emacs-pet` for automatic venv detection.

Install pyright:

```sh
npm install -g pyright
```

Install the Python tree-sitter grammar once inside Emacs:

```
M-x treesit-install-language-grammar -> python
```

`emacs-pet` is installed automatically via `package-selected-packages` on first launch.

**How venv detection works:** pet walks up from the opened file looking for a `.venv`
directory. A `~/.venv` at the home directory serves as the global default — pet finds
it naturally for any file not inside a project that has its own `.venv`.

Create the global default venv (once, after installing `uv`):

```sh
uv venv ~/.venv --python 3.13
```

For a project venv:

```sh
uv venv && uv sync   # run inside the project root
```

## Development Setup

### Node.js

```sh
ln -sfn ~/.unixrc/config/npm/.npmrc ~/.npmrc
```

- `fnm` is used to manage Node.js versions.
- [`config/npm/.npmrc`](config/npm/.npmrc) points npm at `https://registry.npmmirror.com`.

### Python

```sh
mkdir -p ~/.config/pip
ln -sfn ~/.unixrc/config/pip/pip.conf ~/.config/pip/pip.conf
```

- `uv` is the Python toolchain expected by the shell config.
- [`config/pip/pip.conf`](config/pip/pip.conf) points pip at the Tsinghua PyPI mirror.

Install `uv`:

```sh
curl -LsSf https://astral.sh/uv/install.sh | sh
```

Install a Python version and create the global default venv:

```sh
uv python install 3.13
uv venv ~/.venv --python 3.13
```

Shell aliases defined in [`zshrc.d/languages/python.zsh`](zshrc.d/languages/python.zsh):

| Alias   | Description                                      |
|---------|--------------------------------------------------|
| `dvenv` | Activate `~/.venv` in the current shell          |
| `uvdi`  | Install a package into `~/.venv` (`uv pip install --python ~/.venv/bin/python`) |

### Other Development Tools

- `rbenv` is initialized when present locally.
- Go environment defaults live in [`zshrc.d/languages/go.zsh`](zshrc.d/languages/go.zsh).
- Google Cloud SDK, Azure CLI, and CUDA hooks live under [`zshrc.d/tools`](zshrc.d/tools).
