##------------------------------------------------------------------------------
## Cloud Development Environment
##
## Controlled by CLOUD_ENV environment variable
##------------------------------------------------------------------------------

if [[ "${CLOUD_ENV}" == "true" ]]; then
  # Google Cloud SDK
  [[ -f "$HOME/code/source/google-cloud-sdk/path.zsh.inc" ]] && \
    source "$HOME/code/source/google-cloud-sdk/path.zsh.inc"
  [[ -f "$HOME/code/source/google-cloud-sdk/completion.zsh.inc" ]] && \
    source "$HOME/code/source/google-cloud-sdk/completion.zsh.inc"
  
  # Azure CLI
  [[ -f "$HOME/code/source/azure-cli/az.completion" ]] && \
    source "$HOME/code/source/azure-cli/az.completion"
fi

# Additional local environment
[[ -f "$HOME/.local/bin/env" ]] && source "$HOME/.local/bin/env"