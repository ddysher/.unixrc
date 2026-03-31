##------------------------------------------------------------------------------
## Core ZSH Options, Key Bindings, and Defaults
##------------------------------------------------------------------------------

# Disable beeping
setopt NO_BEEP

# Use emacs key bindings
bindkey -e

# Enable colored ls output
export CLICOLOR=1
# Bold blue directories, default files (macOS BSD ls format)
export LSCOLORS="ExGxFxDaCxDaDaHbHdExEx"
# GNU ls / zsh completion colors (bold blue dirs)
export LS_COLORS="di=1;34:ln=1;36:so=1;35:pi=0;33:ex=0;32:bd=0;33:cd=0;33:su=1;37:sg=1;30:tw=1;34:ow=1;34"

# Default editor (overridden by platform-specific configs)
export EDITOR="vim"
