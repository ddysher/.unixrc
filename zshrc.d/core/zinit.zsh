##------------------------------------------------------------------------------
## Zinit Plugin Manager Configuration
##------------------------------------------------------------------------------

export ZINIT_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/zinit/zinit.git"

if [[ ! -r "$ZINIT_HOME/zinit.zsh" ]]; then
  print -P "%F{yellow}[zsh]%f zinit not found at $ZINIT_HOME"
  print -P "%F{yellow}[zsh]%f install it with:"
  print "  git clone --depth=1 https://github.com/zdharma-continuum/zinit.git \"$ZINIT_HOME\""
  return
fi

source "$ZINIT_HOME/zinit.zsh"

# Keep shell startup minimal and defer plugin loading where possible.
zinit ice wait lucid
zinit light zsh-users/zsh-autosuggestions

zinit ice wait lucid
zinit light zsh-users/zsh-syntax-highlighting
