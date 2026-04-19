##------------------------------------------------------------------------------
## Core ZSH Options, Key Bindings, and Defaults
##------------------------------------------------------------------------------

# Disable beeping
setopt NO_BEEP

# Allow typing a directory path to cd into it (needed for global aliases like "...")
setopt AUTO_CD

# Use emacs key bindings
bindkey -e

# Disable XON/XOFF flow control so ctrl-s can be used for forward history search
stty -ixon

# Treat only alphanumeric chars and _ as word characters for M-Backspace / M-f / M-b.
# Removes /, ., -, =, &, ;, |, (, ), <, > from the default so path segments and
# shell operators act as word boundaries.
WORDCHARS='*?_[]~!#$%^{}'

# Enable colored ls output
export CLICOLOR=1
# Bold blue directories, default files (macOS BSD ls format)
export LSCOLORS="ExGxFxDaCxDaDaHbHdExEx"
# GNU ls / zsh completion colors (bold blue dirs)
export LS_COLORS="di=1;34:ln=1;36:so=1;35:pi=0;33:ex=0;32:bd=0;33:cd=0;33:su=1;37:sg=1;30:tw=1;34:ow=1;34"

# History
HISTFILE="$HOME/.zsh_history"
HISTSIZE=50000
SAVEHIST=50000
setopt HIST_IGNORE_DUPS HIST_IGNORE_SPACE INC_APPEND_HISTORY

