##------------------------------------------------------------------------------
## Ruby Development Environment
##------------------------------------------------------------------------------

# rbenv setup
if [ -d "$HOME/.rbenv" ]; then
  path+=("$HOME/.rbenv/bin")
  eval "$(rbenv init -)"
fi
