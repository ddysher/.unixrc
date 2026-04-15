##------------------------------------------------------------------------------
## Kubectl Completion
##------------------------------------------------------------------------------

if command -v kubectl &>/dev/null; then
  local _kubectl_cache="$HOME/.cache/zsh/kubectl-completion-${ZSH_VERSION}.zsh"
  if [[ ! -f "$_kubectl_cache" || $(command -v kubectl) -nt "$_kubectl_cache" ]]; then
    kubectl completion zsh > "$_kubectl_cache"
  fi
  source "$_kubectl_cache"
fi
