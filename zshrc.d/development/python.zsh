##------------------------------------------------------------------------------
## Python Development Environment
##------------------------------------------------------------------------------

# uv setup
[[ -f "$HOME/.local/bin/env" ]] && source "$HOME/.local/bin/env" # added by uv

# use uv managed python globally.
alias python="uv run python"
alias pip="uv pip"
alias venv="uv venv"

export UV_DEFAULT_INDEX="https://pypi.tuna.tsinghua.edu.cn/simple"
