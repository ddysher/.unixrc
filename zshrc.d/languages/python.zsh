##------------------------------------------------------------------------------
## Python Development Environment
##------------------------------------------------------------------------------

# Default venv: ~/.local/share/uv/default-venv, a shared python environment
# Emacs automatically uses it when no project .venv is found (via emacs-pet).
#   Create / recreate:  uv venv ~/.local/share/uv/default-venv --python x.x
#   Activate manually:  dvenv

# uv setup
[[ -f "$HOME/.local/bin/env" ]] && source "$HOME/.local/bin/env" # added by uv

export UV_DEFAULT_INDEX="https://pypi.tuna.tsinghua.edu.cn/simple"
export UV_DEFAULT_VENV="$HOME/.local/share/uv/default-venv"
alias dvenv="source $UV_DEFAULT_VENV/bin/activate"

# Hugging Face mirror for AI/ML development
export HF_ENDPOINT="https://hf-mirror.com"
