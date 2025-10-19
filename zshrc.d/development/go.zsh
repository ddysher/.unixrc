##------------------------------------------------------------------------------
## Go Development Environment
##
## Additional one-time setup:
## $ go env -w GO111MODULE=on
## $ go env -w GOPROXY=https://goproxy.cn,direct
##------------------------------------------------------------------------------

# Go workspace and PATH
export PATH="$PATH:$GOPATH/bin"

# Use locally built Kubernetes binaries if they exist
if [ -d "$GOPATH/src/k8s.io/kubernetes/_output/local/go/bin" ]; then
  export PATH="$GOPATH/src/k8s.io/kubernetes/_output/local/go/bin:$PATH"
fi
