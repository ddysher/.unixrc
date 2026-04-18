##------------------------------------------------------------------------------
## Aliases
##------------------------------------------------------------------------------

# Safe file operations
alias cp="cp -i"
alias mv="mv -i"
alias rm="rm -i"

# Prefer `eza` as a modern `ls` replacement when available.
if command -v eza >/dev/null 2>&1; then
  alias ls="eza --group-directories-first"
  alias ll="eza --long --header --group-directories-first --git"
  alias la="eza --long --all --header --group-directories-first --git"
else
  alias ll="ls -lh"
  alias la="ls -lah"
fi

# Directory navigation (global aliases so they work as arguments too, e.g., "mv abc ...")
# AUTO_CD in shell.zsh makes bare ".."/".." act as cd
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'

# Utility aliases
alias ppj="python -mjson.tool"  # Beautify JSON output
alias sgrep="grep -rnI -C3 --color=always"  # Colorful recursive grep

# Git aliases (similar to ohmyzsh git plugin)
alias gst="git status"
alias gco="git checkout"
alias ga="git add"
alias gcmsg="git commit -m"
alias gp="git push"
alias gl="git pull"

# Coding agents aliases
alias c="claude"
alias o="codex"

# Create directory and cd into it
mkcd() { mkdir -p "$@" && cd "$@"; }

# Docker
docker-ip() { docker inspect --format "{{ .NetworkSettings.IPAddress }}" "$1"; }
docker-pid() { docker inspect --format "{{ .State.Pid }}" "$1"; }

# Platform-specific aliases
case "$(uname)" in
  Darwin)
    alias chrome="open -a Google\ Chrome"
    alias es="TERM=xterm-256color /Applications/Emacs.app/Contents/MacOS/Emacs -nw"
    alias eq="TERM=xterm-256color /Applications/Emacs.app/Contents/MacOS/Emacs -nw -Q"
    alias ec="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n"
    alias emacs="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n"
    ;;
  Linux)
    alias chrome="google-chrome"
    alias es="TERM=xterm-256color /usr/local/bin/emacs -nw"
    alias eq="TERM=xterm-256color /usr/local/bin/emacs -nw -Q"
    alias ec="/usr/local/bin/emacsclient -n"
    alias emacs="/usr/local/bin/emacsclient -n"
    ;;
esac
