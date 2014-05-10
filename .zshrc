##-------------------------------------------------------------------------------
## Zsh configs
##-------------------------------------------------------------------------------
# Change to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load. Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each time
# that oh-my-zsh is loaded.
ZSH_THEME="deyuan"


# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Disable beeping
setopt NO_BEEP

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git python ruby)



##-------------------------------------------------------------------------------
## General configs for all machines
##-------------------------------------------------------------------------------
alias rm="rm -i"
alias mv="mv -i"
alias cp="cp -i"
alias sgrep="grep -rnI -C3 --color=always" # colorful grep

source $ZSH/oh-my-zsh.sh        # Re-exec shell script
bindkey -e                      # Bind keys



##-------------------------------------------------------------------------------
## Configs for Linux and Mac
##-------------------------------------------------------------------------------
if [[ `uname` == "Darwin" ]]; then
    # chrome:  open file in new tab (chrome should already be opened)
    # emacs:   open file in new frame (GUI emacs should already be opened)
    # emacsnw: open terminal emacs
    # emacsserver: open emacs GUI
    alias chrome="open -a Google\ Chrome"
    alias emacs="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n"
    alias emacsnw="TERM=xterm-256color /Applications/Emacs.app/Contents/MacOS/Emacs -nw"
    alias emacsserver="/Applications/Emacs.app/Contents/MacOS/Emacs"
    export LESS=-RFX
    export EDITOR="vim"
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
    alias ppj="python -mjson.tool"
    export EDITOR="vim"
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
    unsetopt correct_all      # do not autocorrect in work machine
elif [[ `hostname` == "watermelon" ]]; then
    export GOPATH=$HOME/code/langs/go # Go path
    export PATH=$PATH:$HOME/.rvm/bin  # Add RVM to PATH for scripting
    export PATH=$PATH:/usr/local/go/bin         # Add go bin directory
    export PATH=$PATH:/usr/class/cs143/cool/bin # Add cool compiler directory
    source ~/code/sources/z/z.sh                # enable z.sh
    # eval `dircolors ~/.dir_colors` # do not using annoying background for 'ls'
fi
