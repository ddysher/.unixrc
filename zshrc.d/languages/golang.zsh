##------------------------------------------------------------------------------
## Go Development Environment
##------------------------------------------------------------------------------

# Go caches and tool binaries
export GOPATH="$HOME/.cache/go"
export GOMODCACHE="$HOME/.cache/go/mod"
export GOBIN="$HOME/.local/bin"

# Go proxy
# or call "$ go env -w GOPROXY=https://goproxy.cn,direct"
export GOPROXY="https://goproxy.io,direct"
