##------------------------------------------------------------------------------
## uv - Python package manager (interactive shell only)
##
## Package index mirrors (UV_DEFAULT_INDEX, PIP_INDEX_URL, HF_ENDPOINT) live
## in ~/.zshenv.
##------------------------------------------------------------------------------

# Bootstrap uv (added by `uv self install`)
[[ -f "$HOME/.local/bin/env" ]] && source "$HOME/.local/bin/env"

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
