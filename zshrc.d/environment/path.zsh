##------------------------------------------------------------------------------
## PATH Management
##------------------------------------------------------------------------------

# Add common development paths
export PATH="$PATH:$HOME/.local/bin"

# CUDA environment
if [ -d /usr/local/cuda/bin ]; then
  export PATH="/usr/local/cuda/bin:$PATH"
fi
