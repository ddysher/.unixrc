##------------------------------------------------------------------------------
## Go Development Environment
##------------------------------------------------------------------------------

# Development workspace
export GOPATH="$HOME/code/workspace"
export CDPATH="${CDPATH:+$CDPATH:}$GOPATH/src"

# Go workspace and PATH
export PATH="${PATH:+$PATH:}$GOPATH/bin"

# Go proxy
# or call "$ go env -w GOPROXY=https://goproxy.cn,direct"
export GOPROXY=https://goproxy.io,direct
