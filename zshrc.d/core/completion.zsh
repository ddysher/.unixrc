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

# Common completion defaults: case-insensitive matching, grouped results,
# and interactive menu selection when multiple candidates exist.
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format '%F{yellow}%d%f'
zstyle ':completion:*:warnings' format '%F{red}no matches found%f'
zstyle ':completion:*' verbose yes

# Use LS_COLORS for file completion coloring, underline the selected item.
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS} 'ma=4'
