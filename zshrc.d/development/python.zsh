##------------------------------------------------------------------------------
## Python Development Environment
##------------------------------------------------------------------------------

# uv setup
[[ -f "$HOME/.local/bin/env" ]] && source "$HOME/.local/bin/env" # added by uv

export UV_DEFAULT_INDEX="https://pypi.tuna.tsinghua.edu.cn/simple"

# Hugging Face mirror for AI/ML development
export HF_ENDPOINT="https://hf-mirror.com"

# add alias to use uv managed python globally.
# note: disable and use "uv python install 3.11 --default"
#
# alias python="uv run python"
# alias pip="uv pip"
# alias venv="uv venv"
