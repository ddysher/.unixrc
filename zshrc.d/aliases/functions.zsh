##------------------------------------------------------------------------------
## Custom Functions
##------------------------------------------------------------------------------

# Create directory and cd into it
alias mkcd='function _mkcd() { mkdir -p "$@" && cd "$@"; }; _mkcd'

# Docker utility functions
alias docker-ip='function _dip() { docker inspect --format "{{ .NetworkSettings.IPAddress }}" "$1"; }; _dip'
alias docker-pid='function _dpid() { docker inspect --format "{{ .State.Pid }}" "$1"; }; _dpid'