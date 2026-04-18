##------------------------------------------------------------------------------
## FZF Fuzzy Finder
##------------------------------------------------------------------------------

# Prefer a faster file source for CTRL-T and plain `fzf` file pickers.
if command -v fd >/dev/null 2>&1; then
  export FZF_DEFAULT_COMMAND='fd --type f --strip-cwd-prefix'
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
elif command -v rg >/dev/null 2>&1; then
  export FZF_DEFAULT_COMMAND="rg --files --hidden --follow --glob '!.git'"
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
fi

# Keep the UI compact and predictable unless the user has already set options.
export FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS:+$FZF_DEFAULT_OPTS }--height=40% --layout=reverse --border"

# Modern fzf can emit its zsh integration directly.
if command -v fzf >/dev/null 2>&1; then
  eval "$(fzf --zsh)"
fi
