##------------------------------------------------------------------------------
## Oh My Zsh Configuration
##------------------------------------------------------------------------------

# Oh My Zsh installation path
export ZSH="$HOME/.unixrc/tools/ohmyzsh"

# Prompt theme is provided by Starship.
export ZSH_THEME=""

# Disable weekly auto-update checks
export DISABLE_AUTO_UPDATE="true"

# Fix eat keybindings in Zsh
if [[ "$TERM" == eat-* ]]; then
  export TERM=xterm-256color
fi

# Plugins to load
plugins=(git python golang vagrant docker kubectl jump)

# Load Oh My Zsh
source "$ZSH/oh-my-zsh.sh"
