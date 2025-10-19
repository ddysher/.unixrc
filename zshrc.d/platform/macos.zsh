##------------------------------------------------------------------------------
## macOS-specific Configuration
##------------------------------------------------------------------------------

# Application aliases
alias chrome="open -a Google\ Chrome"
alias emacs="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n"
alias emacsnw="TERM=xterm-256color /Applications/Emacs.app/Contents/MacOS/Emacs -nw"
alias emacsserver="/Applications/Emacs.app/Contents/MacOS/Emacs"

# Editor configuration
export EDITOR="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"

# Apple Silicon Mac - Homebrew in /opt/homebrew
export HOMEBREW_TEMP="${HOMEBREW_TEMP:-/opt/homebrew/TEMP}"
export HOMEBREW_PREFIX="/opt/homebrew"
export HOMEBREW_CELLAR="/opt/homebrew/Cellar"
export HOMEBREW_REPOSITORY="/opt/homebrew"
export PATH="/opt/homebrew/bin:/opt/homebrew/sbin${PATH+:$PATH}"
export MANPATH="/opt/homebrew/share/man${MANPATH+:$MANPATH}:"
export INFOPATH="/opt/homebrew/share/info:${INFOPATH:-}"
