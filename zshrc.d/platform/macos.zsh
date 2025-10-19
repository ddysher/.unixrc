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

# Homebrew temp directory (default for Intel Macs)
export HOMEBREW_TEMP="${HOMEBREW_TEMP:-/usr/local/TEMP}"

# Go root (default for Intel Macs)
export GOROOT="${GOROOT:-/usr/local/opt/go/libexec}"
