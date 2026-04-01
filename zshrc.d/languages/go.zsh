##------------------------------------------------------------------------------
## Go Development Environment
##------------------------------------------------------------------------------

# Development workspace
export GOPATH="$HOME/code/workspace"
cdpath+=("$GOPATH/src")

# Go workspace and PATH
path+=("$GOPATH/bin")

# Go proxy
# or call "$ go env -w GOPROXY=https://goproxy.cn,direct"
export GOPROXY=https://goproxy.io,direct
