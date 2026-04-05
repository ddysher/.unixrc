##------------------------------------------------------------------------------
## Python Development Environment
##------------------------------------------------------------------------------

# Default venv: ~/.venv, a shared Python 3 environment for scripts and ad-hoc work.
# This works with pet (emacs), which walks up from any file and finds ~/.venv as a
# natural default when no project-level .venv exists.
#   Create / recreate:  uv venv ~/.venv --python 3.x
#   Install packages:   uvdi <pkg>
#   Activate manually:  uvdv

# uv setup
[[ -f "$HOME/.local/bin/env" ]] && source "$HOME/.local/bin/env" # added by uv

export UV_DEFAULT_INDEX="https://pypi.tuna.tsinghua.edu.cn/simple"
alias uvdv="source $HOME/.venv/bin/activate"
alias uvdi="uv pip install --python $HOME/.venv/bin/python"

# PIP index URL
export PIP_INDEX_URL=https://pypi.tuna.tsinghua.edu.cn/simple

# Hugging Face mirror for AI/ML development
export HF_ENDPOINT="https://hf-mirror.com"
