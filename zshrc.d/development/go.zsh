##------------------------------------------------------------------------------
## Go Development Environment
##
## Additional one-time setup:
## $ go env -w GO111MODULE=on
## $ go env -w GOPROXY=https://goproxy.cn,direct
##------------------------------------------------------------------------------

# Development workspace
export GOPATH="$HOME/code/workspace"
export CDPATH="$CDPATH:$GOPATH/src"

# Go workspace and PATH
export PATH="$PATH:$GOPATH/bin"

# Go root (default for Intel Macs)
if [[ "$(uname)" == "Darwin" ]]; then
  export GOROOT="${GOROOT:-/usr/local/opt/go/libexec}"
fi
