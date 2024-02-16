##-------------------------------------------------------------------------------
## Switches
##-------------------------------------------------------------------------------
# Enable/Disable cloud envs which contains cli, libraries for gcp, azure, etc
CLOUD_ENV=${CLOUD_ENV:-"false"} # "true" or "false"

##-------------------------------------------------------------------------------
## Zsh configs
##-------------------------------------------------------------------------------
# Change to your ohmyzsh configuration.
ZSH=$HOME/.unixrc/ohmyzsh

# Set name of the theme to load. Look in ~/.ohmyzsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each time
# that ohmyzsh is loaded.
ZSH_THEME="deyuan"

# Figure out the SHORT hostname.
# This is copied from ohmyzsh.sh to move the location of the current completion
# dump file under ~/.cache/zsh, otherwise there will be a lot of .zcompdump files
# under home directory.
if [[ "$OSTYPE" = darwin* ]]; then
  # macOS's $HOST changes with dhcp, etc. Use ComputerName if possible.
  SHORT_HOST=$(scutil --get ComputerName 2>/dev/null) || SHORT_HOST=${HOST/.*/}
else
  SHORT_HOST=${HOST/.*/}
fi
[[ ! -d "$HOME/.cache/zsh" ]] && mkdir $HOME/.cache/zsh
ZSH_COMPDUMP=$HOME/.cache/zsh/.zcompdump-${SHORT_HOST}-${ZSH_VERSION}

# Disable weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Display red dots while waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Disable beeping
setopt NO_BEEP

# Which plugins would you like to load? (plugins can be found in $ZSH/plugins/*)
# Custom plugins may be added to $ZSH/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git python golang vagrant docker kubectl jump)

##------------------------------------------------------------------------------
## Special configs that must run at first
##------------------------------------------------------------------------------
if [[ `uname` == "Darwin" ]]; then
  if [[ `arch` == "i386" ]]; then
    # Homebrew requires "/usr/local/bin" and "/usr/local/sbin", and both
    # directories should appear before "/usr/bin".
    export PATH=/usr/local/sbin:$PATH
    export PATH=/usr/local/bin:$PATH
  fi

  if [[ `arch` == "arm64" ]]; then
    export HOMEBREW_PREFIX="/opt/homebrew";
    export HOMEBREW_CELLAR="/opt/homebrew/Cellar";
    export HOMEBREW_REPOSITORY="/opt/homebrew";
    export PATH="/opt/homebrew/bin:/opt/homebrew/sbin${PATH+:$PATH}";
    export MANPATH="/opt/homebrew/share/man${MANPATH+:$MANPATH}:";
    export INFOPATH="/opt/homebrew/share/info:${INFOPATH:-}";
  fi
fi

##------------------------------------------------------------------------------
## General configs for all machines
##------------------------------------------------------------------------------
alias cp="cp -i"
alias lg="ll --group-directories-first"
alias mv="mv -i"
alias pc="proxychains4"
alias ppj="python -mjson.tool"  # Beautify json print
alias rm="rm -i"
alias sgrep="grep -rnI -C3 --color=always" # Colorful grep
alias drm="docker rm"
alias dps="docker ps"
alias j="jump"
alias cnpm="npm --registry=https://registry.npm.taobao.org \
--cache=$HOME/.npm/.cache/cnpm \
--disturl=https://npm.taobao.org/dist \
--userconfig=$HOME/.cnpmrc"
alias mkcd='function _mkcd() { mkdir -p "$@" && cd "$@"; }; _mkcd'
alias docker-ip='function _dip() { docker inspect --format "{{ .NetworkSettings.IPAddress }}" $1; }; _dip'
alias docker-pid='function _dpid() { docker inspect --format "{{ .State.Pid }}" $1; }; _dpid'

source $ZSH/oh-my-zsh.sh          # Re-exec shell script
source $HOME/.unixrc/tools/z/z.sh # Enable z.sh
bindkey -e                        # Bind keys

# Add misc useful scripts to PATH.
export PATH=$PATH:$HOME/code/tools/scripts

##-------------------------------------------------------------------------------
## Configs for Linux and Mac
##   chrome:  open file in new tab (chrome should already be opened)
##   emacs:   open file in new frame (GUI emacs should already be opened)
##   emacsnw: open terminal emacs
##   emacsserver: open emacs GUI
##-------------------------------------------------------------------------------
if [[ `uname` == "Darwin" ]]; then
  alias chrome="open -a Google\ Chrome"
  alias emacs="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n"
  alias emacsnw="TERM=xterm-256color /Applications/Emacs.app/Contents/MacOS/Emacs -nw"
  alias emacsserver="/Applications/Emacs.app/Contents/MacOS/Emacs"
  # Need full path for EDITOR variable in OSX.
  export EDITOR="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
  export HOMEBREW_TEMP=/usr/local/TEMP
  export GOROOT=/usr/local/opt/go/libexec
elif [[ `uname` == "Linux" ]]; then
  alias chrome="google-chrome"
  alias emacs="/usr/local/bin/emacsclient -n"
  alias emacsnw="TERM=xterm-256color /usr/local/bin/emacs -nw"
  alias emacsserver="/usr/local/bin/emacs"
  export EDITOR="emacsclient"     # TODO: make sure emacs server started
fi

##-------------------------------------------------------------------------------
## Configs for different hosts
##-------------------------------------------------------------------------------
if [[ `hostname` == "Deyuans-Macbook-Air.local" ]]; then
  alias mysql=/usr/local/mysql/bin/mysql
  alias mysqladmin=/usr/local/mysql/bin/mysqladmin
  alias mysqld_safe=/usr/local/mysql/bin/mysqld_safe
elif [[ `hostname` == "Deyuans-MacBook-Pro.local" ]]; then
  # Due to MacOS SIP (System Integration Protection), installing python libraries
  # will be rejected if it touches '/System' directory. It is thus recommended to
  # install per user, i.e. "pip install ipython --user". This is needed for MacOS
  # ElCapitan. The path is updated to include the binaries installed for current
  # user. Use "python -m site" to see all system path.
  # Update: comment this out in favor of pyenv.
  # export PATH=$PATH:$HOME/Library/Python/2.7/bin
elif [[ `hostname` == "deyuan.pit.corp.google.com" ]]; then
  unsetopt correct_all          # Do not autocorrect
  export P4EDITOR="emacsclient"
  source /etc/bash_completion.d/g4d
elif [[ `hostname` == "deyuan-macbookpro.roam.corp.google.com" ]]; then
elif [[ `hostname` == "watermelon" ]]; then
  # Do not using annoying background for 'ls'
  # eval `dircolors ~/.dir_colors`
elif [[ `hostname` == "Deyuans-MacBook-M1" ]]; then
  export HOMEBREW_TEMP=/opt/homebrew/TEMP
  export GOROOT=/opt/homebrew/opt/go/libexec
elif [[ `hostname` == "Deyuans-MacBook-Pro-16" ]]; then
  export HOMEBREW_TEMP=/opt/homebrew/TEMP
  export GOROOT=/opt/homebrew/opt/go/libexec
  # source /Users/deyuandeng/.docker/init-zsh.sh || true # Added by Docker Desktop
fi

##------------------------------------------------------------------------------
##
## Followings are development related configuration across all hosts.
##
##------------------------------------------------------------------------------

##----------------------------------------------------------
## Setup Go and Cloud Native environment.
## - Export global PATH environmennt, etc
##
## Additional one-time setup
## $ go env -w GO111MODULE=on
## $ go env -w GOPROXY=https://goproxy.cn,direct

export GOPATH=$HOME/code/workspace
export CDPATH=$CDPATH:$GOPATH/src
export PATH=$PATH:$GOPATH/bin

if [[ -x $(command -v kubectl) ]]; then
  source <(kubectl completion zsh)
fi

# Use locally built Kubernetes binaries if exists.
if [ -d $GOPATH/src/k8s.io/kubernetes/_output/local/go/bin ]; then
  export PATH=$GOPATH/src/k8s.io/kubernetes/_output/local/go/bin:$PATH
fi

## End of Go and Cloud Native setup.
##----------------------------------------------------------

##----------------------------------------------------------
## Setup Python and ML environment.
## - Use pyenv to manage multiple Python versions.
## - Use pipx to install global Python cli and applications.
## - Use python3 venv to manage virtual environments.
## - Set Hugging Face mirror to CN domain.
##
## Additional one-time setup:
## $ pip config set global.index-url https://pypi.tuna.tsinghua.edu.cn/simple

# Add pyenv executable to PATH and enable shims, then load pyenv into the shell.
if [ -d $HOME/.pyenv ]; then
  export PYENV_ROOT="$HOME/.pyenv"
  export PATH="$PYENV_ROOT/bin:$PATH"
  eval "$(pyenv init --path)"
  eval "$(pyenv init -)"
fi

# Setups when pipx is installed:
# - Remove ipython alias set via zsh python plugin, which conflicts with pipx installed ipython.
# - Add .local/bin to PATH, which is the directory where pipx installs isolated apps.
if [[ -x $(command -v pipx) ]]; then
  unalias ipython
  export PATH=$PATH:$HOME/.local/bin
fi

# Using python3 venv module to manage virtual environment.
export VENV_HOME="$HOME/.venv"
[[ -d $VENV_HOME ]] || mkdir $VENV_HOME

function lsvenv() {
  ls -1 $VENV_HOME
}

function venv() {
  if [ $# -eq 0 ]
  then
    echo "Please provide venv name"
  else
    source "$VENV_HOME/$1/bin/activate"
  fi
}

function mkvenv() {
  if [ $# -eq 0 ]
  then
    echo "Please provide venv name"
  else
    echo "Creating venv under $VENV_HOME/$1"
    python3 -m venv $VENV_HOME/$1
    echo "Activating $1"
    venv $1
  fi
}

function rmvenv() {
  if [ $# -eq 0 ]
  then
    echo "Please provide venv name"
  else
    rm -rf $VENV_HOME/$1
  fi
}

complete -C lsvenv venv
complete -C lsvenv rmvenv

# Set huggingface mirror.
export HF_ENDPOINT=https://hf-mirror.com

## End of Python setup.
##----------------------------------------------------------

##----------------------------------------------------------
## Setup Java, Ruby, Cloud, etc.

# Java environment for Mac with brew.
# sudo ln -sfn /opt/homebrew/opt/openjdk/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk.jdk
# echo 'export PATH="/opt/homebrew/opt/openjdk/bin:$PATH"' >> ~/.zshrc
# export CPPFLAGS="-I/opt/homebrew/opt/openjdk/include"

# Ruby environment.
if [ -d $HOME/.rbenv ]; then
  export PATH=$PATH:$HOME/.rbenv/bin
  eval "$(rbenv init -)"
fi

# Cloud environment.
if [[ "${CLOUD_ENV}" == "true" ]]; then
  [[ -f "$HOME/code/source/google-cloud-sdk/path.zsh.inc" ]] && source "$HOME/code/source/google-cloud-sdk/path.zsh.inc"
  [[ -f "$HOME/code/source/google-cloud-sdk/path.zsh.inc" ]] && source "$HOME/code/source/google-cloud-sdk/completion.zsh.inc"
  [[ -f "$HOME/code/source/azure-cli/az.completion" ]] && source "$HOME/code/source/azure-cli/az.completion"
fi

## End of misc setup.
##----------------------------------------------------------
