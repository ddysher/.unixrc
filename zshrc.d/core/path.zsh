##------------------------------------------------------------------------------
## Homebrew completion paths (interactive shells only)
##
## PATH and HOMEBREW_PREFIX are already set in ~/.zshenv. This file only adds
## FPATH and INFOPATH which are needed for zsh completions and info pages.
##------------------------------------------------------------------------------

typeset -gU fpath FPATH infopath INFOPATH

[[ -n "$HOMEBREW_PREFIX" ]] && {
  export FPATH="$HOMEBREW_PREFIX/share/zsh/site-functions${FPATH:+:$FPATH}"
  export INFOPATH="$HOMEBREW_PREFIX/share/info${INFOPATH:+:$INFOPATH}"
}
