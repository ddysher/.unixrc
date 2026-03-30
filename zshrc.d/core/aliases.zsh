##------------------------------------------------------------------------------
## Aliases
##------------------------------------------------------------------------------

# Safe file operations
alias cp="cp -i"
alias mv="mv -i"
alias rm="rm -i"

# Enhanced ls
alias ll="ls -lh"
alias lg="ls -lh --group-directories-first"

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
