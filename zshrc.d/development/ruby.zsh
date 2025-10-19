##------------------------------------------------------------------------------
## Ruby Development Environment
##------------------------------------------------------------------------------

# rbenv setup
if [ -d "$HOME/.rbenv" ]; then
  export PATH="$PATH:$HOME/.rbenv/bin"
  eval "$(rbenv init -)"
fi
