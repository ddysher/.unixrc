##------------------------------------------------------------------------------
## Aliases
##------------------------------------------------------------------------------

# Safe file operations
alias cp="cp -i"
alias mv="mv -i"
alias rm="rm -i"

# Enhanced ls
alias ll="ls -lh"
alias lg="ls -lh"

# Directory navigation (global aliases so they work as arguments too, e.g., "mv abc ...")
# AUTO_CD in shell.zsh makes bare ".."/".." act as cd
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'

# Utility aliases
alias ppj="python -mjson.tool"  # Beautify JSON output
alias sgrep="grep -rnI -C3 --color=always"  # Colorful recursive grep

# Create directory and cd into it
alias mkcd='function _mkcd() { mkdir -p "$@" && cd "$@"; }; _mkcd'

# Git aliases (similar to ohmyzsh git plugin)
alias gst="git status"
alias gco="git checkout"
alias ga="git add"
alias gcmsg="git commit -m"
alias gp="git push"
alias gl="git pull"

# Docker
alias docker-ip='function _dip() { docker inspect --format "{{ .NetworkSettings.IPAddress }}" "$1"; }; _dip'
alias docker-pid='function _dpid() { docker inspect --format "{{ .State.Pid }}" "$1"; }; _dpid'

# Platform-specific aliases
case "$(uname)" in
  Darwin)
    alias chrome="open -a Google\ Chrome"
    alias e="TERM=xterm-256color /Applications/Emacs.app/Contents/MacOS/Emacs -nw"
    alias eq="TERM=xterm-256color /Applications/Emacs.app/Contents/MacOS/Emacs -nw -Q"
    alias ec="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n"
    alias emacs="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n"
    ;;
  Linux)
    alias chrome="google-chrome"
    alias e="TERM=xterm-256color /usr/local/bin/emacs -nw"
    alias eq="TERM=xterm-256color /usr/local/bin/emacs -nw -Q"
    alias ec="/usr/local/bin/emacsclient -n"
    alias emacs="/usr/local/bin/emacsclient -n"
    ;;
esac
