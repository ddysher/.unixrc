##------------------------------------------------------------------------------
## Python Development Environment
##------------------------------------------------------------------------------

# Bootstrap uv (added by `uv self install`)
[[ -f "$HOME/.local/bin/env" ]] && source "$HOME/.local/bin/env"

##------------------------------------------------------------------------------
## Package Index (mirrors for faster access in China)
##------------------------------------------------------------------------------

# uv default index (replaces deprecated UV_INDEX_URL)
export UV_DEFAULT_INDEX="https://pypi.tuna.tsinghua.edu.cn/simple"

# pip fallback index (for tools that invoke pip directly)
export PIP_INDEX_URL="https://pypi.tuna.tsinghua.edu.cn/simple"

# Hugging Face model/dataset mirror
export HF_ENDPOINT="https://hf-mirror.com"

##------------------------------------------------------------------------------
## Shared virtualenv (~/.venv)
##------------------------------------------------------------------------------
#
# A single shared Python 3 environment for scripts and ad-hoc work.
# Emacs pet walks up from any file and uses ~/.venv as the default
# when no project-level .venv exists.
#
#   Create / recreate:  uv venv ~/.venv --python 3.x
#   Activate manually:  uvdv
#   Install a package:  uvdi <pkg>

alias uvdv="source $HOME/.venv/bin/activate"
alias uvdi="uv pip install --python $HOME/.venv/bin/python"
