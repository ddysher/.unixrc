##------------------------------------------------------------------------------
## Configuration for Deyuans-MacBook-Pro.local
##------------------------------------------------------------------------------

# Historical note about MacOS SIP and Python paths
# Due to MacOS SIP (System Integration Protection), installing python libraries
# will be rejected if it touches '/System' directory. It is thus recommended to
# install per user, i.e. "pip install ipython --user". This is needed for MacOS
# ElCapitan. The path is updated to include the binaries installed for current
# user. Use "python -m site" to see all system path.
# Update: comment this out in favor of pyenv.
# export PATH=$PATH:$HOME/Library/Python/2.7/bin