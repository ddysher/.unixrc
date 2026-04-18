##------------------------------------------------------------------------------
## Core PATH setup
##------------------------------------------------------------------------------

# Ensure no duplicates in path-like vars. This file owns the baseline PATH
# layout, including Homebrew on macOS. Other modules may still append their
# own tool-specific entries later, so keep those additions minimal and local.
typeset -gU path PATH fpath FPATH infopath INFOPATH

# User local bin
export PATH="$HOME/.local/bin:$PATH"

# Homebrew environment and search paths (macOS only). Keeping this in `core`
# avoids sourcing a tool module before `core/completion.zsh`, which needs
# Homebrew's zsh completion directory in FPATH before `compinit` runs.
[[ "$(uname)" == "Darwin" ]] || return

if [[ -x /opt/homebrew/bin/brew ]]; then
  export HOMEBREW_PREFIX="/opt/homebrew"
elif [[ -x /usr/local/bin/brew ]]; then
  export HOMEBREW_PREFIX="/usr/local"
elif command -v brew >/dev/null 2>&1; then
  export HOMEBREW_PREFIX="$(brew --prefix)"
else
  return
fi

export HOMEBREW_CELLAR="$HOMEBREW_PREFIX/Cellar"
export HOMEBREW_REPOSITORY="$HOMEBREW_PREFIX"
export HOMEBREW_NO_ENV_HINTS=1

export PATH="$HOMEBREW_PREFIX/bin:$HOMEBREW_PREFIX/sbin:$PATH"
export FPATH="$HOMEBREW_PREFIX/share/zsh/site-functions${FPATH:+:$FPATH}"
export INFOPATH="$HOMEBREW_PREFIX/share/info${INFOPATH:+:$INFOPATH}"
