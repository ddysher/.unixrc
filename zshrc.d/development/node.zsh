##------------------------------------------------------------------------------
## Node.js Development Environment
##------------------------------------------------------------------------------

# Fast Node Manager (fnm) setup, used to manage multiple nodejs versions.
FNM_PATH="/opt/homebrew/opt/fnm/bin"
if [ -d "$FNM_PATH" ]; then
  eval "$(fnm env)"
fi

# Chinese npm registry alias
alias cnpm="npm --registry=https://registry.npm.taobao.org \
--cache=$HOME/.npm/.cache/cnpm \
--disturl=https://npm.taobao.org/dist \
--userconfig=$HOME/.cnpmrc"
