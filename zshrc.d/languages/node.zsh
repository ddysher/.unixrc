##------------------------------------------------------------------------------
## Node.js Development Environment
##------------------------------------------------------------------------------

# Fast Node Manager (fnm) setup, used to manage multiple nodejs versions.
case "$(uname)" in
  Darwin) FNM_BIN="/opt/homebrew/opt/fnm/bin/fnm" ;;
  Linux)  FNM_BIN="$HOME/.local/share/fnm/fnm" ;;
  *)      FNM_BIN="" ;;
esac

if command -v fnm &>/dev/null; then
  eval "$(fnm env)"
elif [[ -x "$FNM_BIN" ]]; then
  export PATH="${FNM_BIN%/*}:$PATH"
  eval "$("$FNM_BIN" env)"
fi

# Set npm registry
export NPM_CONFIG_REGISTRY=https://registry.npmmirror.com/
