##------------------------------------------------------------------------------
## ZSH Completion Configuration
##------------------------------------------------------------------------------

# Figure out the SHORT hostname for completion dump file
case "$(uname)" in
  Darwin) SHORT_HOST=$(scutil --get ComputerName 2>/dev/null) || SHORT_HOST=${HOST/.*/} ;;
  *)      SHORT_HOST=${HOST/.*/} ;;
esac

# Create cache directory and set completion dump location
[[ ! -d "$HOME/.cache/zsh" ]] && mkdir -p "$HOME/.cache/zsh"
export ZSH_COMPDUMP="$HOME/.cache/zsh/.zcompdump-${SHORT_HOST}-${ZSH_VERSION}"

autoload -Uz compinit
# Only run compaudit (security check) if the dump is older than 24 hours.
if [[ -n "$ZSH_COMPDUMP"(#qN.mh+24) ]]; then
  compinit -d "$ZSH_COMPDUMP"
else
  compinit -C -d "$ZSH_COMPDUMP"
fi

# Completion settings
export COMPLETION_WAITING_DOTS="true"

# Enable menu selection for tab cycling
zstyle ':completion:*' menu select
# Use LS_COLORS for file completion coloring, underline selected item (ma=4)
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS} 'ma=4'
