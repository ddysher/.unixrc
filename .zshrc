##-------------------------------------------------------------------------------
## Zsh configs
##-------------------------------------------------------------------------------
# Change to your oh-my-zsh configuration.
ZSH=$HOME/.unixrc/oh-my-zsh

# Set name of the theme to load. Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each time
# that oh-my-zsh is loaded.
ZSH_THEME="deyuan"

# Change to pkg download directory.
TOOLS=$HOME/.unixrc/tools

# Set to this to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting
# for completion.
COMPLETION_WAITING_DOTS="true"

# Disable beeping
setopt NO_BEEP

# Which plugins would you like to load? (plugins can be found in $ZSH/plugins/*)
# Custom plugins may be added to $ZSH/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git python ruby)



##------------------------------------------------------------------------------
## General configs for all machines
##------------------------------------------------------------------------------
alias rm="rm -i"
alias mv="mv -i"
alias cp="cp -i"
alias lg="ll --group-directories-first"
alias ppj="python -mjson.tool"             # Beautify json print
alias sgrep="grep -rnI -C3 --color=always" # Colorful grep

source $ZSH/oh-my-zsh.sh        # Re-exec shell script
source $TOOLS/z/z.sh            # Enable z.sh
bindkey -e                      # Bind keys



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
  export LESS=-RFX
  export EDITOR="emacsclient"
  export TOMCAT_HOME=/Library/Tomcat
  export HOMEBREW_TEMP=/usr/local/TEMP
  export CLASSPATH=.:/Library/Java/Extensions
  export PATH="/usr/local/heroku/bin:$PATH" # Added by the Heroku Toolbelt
  source `brew --prefix`/etc/profile.d/z.sh
elif [[ `uname` == "Linux" ]]; then
  alias chrome="google-chrome"
  alias emacs="/usr/local/bin/emacsclient -n"
  alias emacsnw="TERM=xterm-256color /usr/local/bin/emacs -nw"
  alias emacsserver="/usr/local/bin/emacs"
  export EDITOR="emacsclient"
  export PATH=$PATH:$TOOLS/arcanist/bin:/usr/local/go/bin
  export GOPATH=$HOME/code/source/go-workspace
  export PATH=$PATH:$HOME/code/source/go-workspace/bin
fi



##-------------------------------------------------------------------------------
## Configs for different hosts
##-------------------------------------------------------------------------------
if [[ `hostname` == "Deyuans-Macbook-Air.local" ]]; then
  alias mysql=/usr/local/mysql/bin/mysql
  alias mysqladmin=/usr/local/mysql/bin/mysqladmin
  alias mysqld_safe=/usr/local/mysql/bin/mysqld_safe
elif [[ `hostname` == "deyuan.pit.corp.google.com" ]]; then
  export P4EDITOR="emacsclient"
  source /etc/bash_completion.d/g4d
  unsetopt correct_all      # Do not autocorrect
elif [[ `hostname` == "watermelon" ]]; then
  # source '/home/deyuan/code/source/google-cloud-sdk/path.zsh.inc'
  # source '/home/deyuan/code/source/google-cloud-sdk/completion.zsh.inc'
  # eval `dircolors ~/.dir_colors` # do not using annoying background for 'ls'
fi
