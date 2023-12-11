export HISTFILE="$HOME/.zsh_history"
export SHOW_BRANCH_IN_PROMPT=1
export PAR_DIR_PRINT_LIMIT=2
bindkey -v
setopt SHARE_HISTORY

for file in "$ZDOTDIR"/*.sh; do
    source "$file"
done



