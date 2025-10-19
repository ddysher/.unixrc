##------------------------------------------------------------------------------
## Homebrew Environment Setup
##------------------------------------------------------------------------------

if [[ "$(uname)" == "Darwin" ]]; then
  if [[ "$(arch)" == "i386" ]]; then
    # Intel Mac - Homebrew in /usr/local
    export PATH="/usr/local/sbin:$PATH"
    export PATH="/usr/local/bin:$PATH"
  elif [[ "$(arch)" == "arm64" ]]; then
    # Apple Silicon Mac - Homebrew in /opt/homebrew
    export HOMEBREW_PREFIX="/opt/homebrew"
    export HOMEBREW_CELLAR="/opt/homebrew/Cellar"
    export HOMEBREW_REPOSITORY="/opt/homebrew"
    export PATH="/opt/homebrew/bin:/opt/homebrew/sbin${PATH+:$PATH}"
    export MANPATH="/opt/homebrew/share/man${MANPATH+:$MANPATH}:"
    export INFOPATH="/opt/homebrew/share/info:${INFOPATH:-}"
  fi
fi
