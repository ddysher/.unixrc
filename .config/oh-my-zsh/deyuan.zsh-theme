# Format for git_prompt_info()
ZSH_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}on %{$fg_bold[magenta]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"

# Format for parse_git_dirty()
ZSH_THEME_GIT_PROMPT_DIRTY="! "
ZSH_THEME_GIT_PROMPT_CLEAN=" "

# Format for git_prompt_status()
ZSH_THEME_GIT_PROMPT_UNMERGED=" %{$fg[red]%}unmerged"
ZSH_THEME_GIT_PROMPT_DELETED=" %{$fg[red]%}deleted"
ZSH_THEME_GIT_PROMPT_RENAMED=" %{$fg[yellow]%}renamed"
ZSH_THEME_GIT_PROMPT_MODIFIED=" %{$fg[yellow]%}modified"
ZSH_THEME_GIT_PROMPT_ADDED=" %{$fg[green]%}added"
ZSH_THEME_GIT_PROMPT_UNTRACKED=" %{$fg[white]%}untracked"

# # Format for git_prompt_ahead()
ZSH_THEME_GIT_PROMPT_AHEAD=" %{$fg[red]%}(!)"

# # Format for git_prompt_long_sha() and git_prompt_short_sha()
ZSH_THEME_GIT_PROMPT_SHA_BEFORE=" %{$fg[white]%}[%{$fg[yellow]%}"
ZSH_THEME_GIT_PROMPT_SHA_AFTER="%{$fg[white]%}]"

# Prompt format
if [[ `hostname` == "orange" ]]; then
   PROMPT='%{$fg_bold[magenta]%}$USER %{$reset_color%}at %{$fg_bold[yellow]%}%m %{$reset_color%}in %{$fg_bold[green]%}${PWD/#$HOME/~}%u $(git_prompt_info)% %{$fg_bold[magenta]%}$ %{$reset_color%}'
else
   PROMPT='%{$fg_bold[magenta]%}$USER %{$reset_color%}at %{$fg_bold[yellow]%}%m %{$reset_color%}in %{$fg_bold[green]%}${PWD/#$HOME/~}%u $(git_prompt_info)% %{$fg_bold[magenta]%}
$ %{$reset_color%}'
fi
