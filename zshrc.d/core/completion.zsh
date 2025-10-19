##------------------------------------------------------------------------------
## ZSH Completion Configuration
##------------------------------------------------------------------------------

# Figure out the SHORT hostname for completion dump file
if [[ "$OSTYPE" = darwin* ]]; then
  SHORT_HOST=$(scutil --get ComputerName 2>/dev/null) || SHORT_HOST=${HOST/.*/}
else
  SHORT_HOST=${HOST/.*/}
fi

# Create cache directory and set completion dump location
[[ ! -d "$HOME/.cache/zsh" ]] && mkdir -p "$HOME/.cache/zsh"
export ZSH_COMPDUMP="$HOME/.cache/zsh/.zcompdump-${SHORT_HOST}-${ZSH_VERSION}"

# Completion settings
export COMPLETION_WAITING_DOTS="true"

# Load completions for installed tools
if [[ -x $(command -v kubectl) ]]; then
  source <(kubectl completion zsh)
fi
