##------------------------------------------------------------------------------
## Python Development Environment
##
## Additional one-time setup:
## $ pip config set global.index-url https://pypi.tuna.tsinghua.edu.cn/simple
##------------------------------------------------------------------------------

# uv setup
[[ -f "$HOME/.local/bin/env" ]] && source "$HOME/.local/bin/env" # added by uv

# use uv managed python globally.
alias python="uv run python"
alias pip="uv pip"


# pyenv setup, used to manage multiple python versions (retiring).
if [ -d "$HOME/.pyenv" ]; then
  export PYENV_ROOT="$HOME/.pyenv"
  export PATH="$PYENV_ROOT/bin:$PATH"
  eval "$(pyenv init --path)"
  eval "$(pyenv init -)"
fi


# pipx setup, used to install global python cli tools and applications
if [[ -x $(command -v pipx) ]]; then
  # Remove ipython alias set via zsh python plugin to avoid conflicts
  unalias ipython 2>/dev/null || true
  export PATH="$PATH:$HOME/.local/bin"
fi


# python virtual environment home.
export VENV_HOME="$HOME/.venv"
[[ -d $VENV_HOME ]] || mkdir -p $VENV_HOME

# Virtual environment management functions
function lsvenv() {
  ls -1 "$VENV_HOME"
}

function venv() {
  if [ $# -eq 0 ]; then
    echo "Please provide venv name"
  else
    source "$VENV_HOME/$1/bin/activate"
  fi
}

function mkvenv() {
  if [ $# -eq 0 ]; then
    echo "Please provide venv name"
  else
    echo "Creating venv under $VENV_HOME/$1"
    python3 -m venv "$VENV_HOME/$1"
    echo "Activating $1"
    venv "$1"
  fi
}

function rmvenv() {
  if [ $# -eq 0 ]; then
    echo "Please provide venv name"
  else
    rm -rf "$VENV_HOME/$1"
  fi
}

# Completions for venv functions
complete -C lsvenv venv
complete -C lsvenv rmvenv
