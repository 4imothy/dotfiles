export HISTFILE="$HOME/.zsh_history"
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'
bindkey -v
setopt SHARE_HISTORY

for file in "$ZDOTDIR"/*.sh; do
    source "$file"
done
