##------------------------------------------------------------------------------
## Oh My Zsh Configuration
##------------------------------------------------------------------------------

# Oh My Zsh installation path
export ZSH="$HOME/.unixrc/ohmyzsh"

# Theme
export ZSH_THEME="deyuan"

# Disable weekly auto-update checks
export DISABLE_AUTO_UPDATE="true"

# Plugins to load
plugins=(git python golang vagrant docker kubectl jump)

# Load Oh My Zsh
source "$ZSH/oh-my-zsh.sh"
