##------------------------------------------------------------------------------
## Python Development Environment
##------------------------------------------------------------------------------

# Default venv: ~/.venv, a shared Python 3.13 environment for scripts and ad-hoc work.
# pet (emacs) walks up from any file and finds ~/.venv as a natural fallback
# when no project-level .venv exists — no custom Emacs logic needed.
#   Create / recreate:  uv venv ~/.venv --python 3.13
#   Install packages:   uvdi <pkg>
#   Activate manually:  dvenv

# uv setup
[[ -f "$HOME/.local/bin/env" ]] && source "$HOME/.local/bin/env" # added by uv

export UV_DEFAULT_INDEX="https://pypi.tuna.tsinghua.edu.cn/simple"
alias dvenv="source $HOME/.venv/bin/activate"
alias uvdi="uv pip install --python $HOME/.venv/bin/python"

# Hugging Face mirror for AI/ML development
export HF_ENDPOINT="https://hf-mirror.com"
