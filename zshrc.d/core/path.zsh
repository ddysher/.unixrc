##------------------------------------------------------------------------------
## Core PATH setup
##------------------------------------------------------------------------------

# Ensure no duplicates in PATH
typeset -gU PATH

# User local bin
export PATH="$HOME/.local/bin:$PATH"

# Homebrew (macOS)
if [[ "$(uname)" == "Darwin" ]]; then
  export HOMEBREW_TEMP="${HOMEBREW_TEMP:-/opt/homebrew/TEMP}"
  export HOMEBREW_PREFIX="/opt/homebrew"
  export HOMEBREW_CELLAR="/opt/homebrew/Cellar"
  export HOMEBREW_REPOSITORY="/opt/homebrew"
  export HOMEBREW_NO_ENV_HINTS=1
  export PATH="$HOMEBREW_PREFIX/bin:$HOMEBREW_PREFIX/sbin:$PATH"
  export MANPATH="$HOMEBREW_PREFIX/share/man:$MANPATH"
  export INFOPATH="$HOMEBREW_PREFIX/share/info${INFOPATH+:$INFOPATH}"
fi
