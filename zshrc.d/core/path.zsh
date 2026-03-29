##------------------------------------------------------------------------------
## Core PATH and directory search setup
##------------------------------------------------------------------------------

typeset -gU path PATH cdpath CDPATH manpath MANPATH

path=(
  "$HOME/.local/bin"
  $path
)

case "$(uname)" in
  Darwin)
    export HOMEBREW_TEMP="${HOMEBREW_TEMP:-/opt/homebrew/TEMP}"
    export HOMEBREW_PREFIX="/opt/homebrew"
    export HOMEBREW_CELLAR="/opt/homebrew/Cellar"
    export HOMEBREW_REPOSITORY="/opt/homebrew"
    path=(
      "$HOMEBREW_PREFIX/bin"
      "$HOMEBREW_PREFIX/sbin"
      $path
    )
    manpath=(
      "$HOMEBREW_PREFIX/share/man"
      $manpath
    )
    export INFOPATH="$HOMEBREW_PREFIX/share/info${INFOPATH+:$INFOPATH}"
    ;;
esac
