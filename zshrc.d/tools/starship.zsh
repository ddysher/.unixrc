##------------------------------------------------------------------------------
## Starship Prompt
##------------------------------------------------------------------------------

# Initialize Starship only when the binary is available.
if command -v starship >/dev/null 2>&1; then
  eval "$(starship init zsh)"
fi
