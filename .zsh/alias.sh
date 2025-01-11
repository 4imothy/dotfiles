alias c="clear"
alias ch="history -p; clear"
alias ct='clear; history -p; clear; tmux clear-history; clear'
alias tg="tgrep"
alias venv="source venv/bin/activate"
alias todom="todo -m"
alias tree="tgrep --tree"
alias ltree="tgrep --tree --long-branch"
alias run="python3 run.py"
alias reload="source $ZDOTDIR/.zshrc"

alias gs='git status'
alias ga='git add'
alias gc='git commit'
alias gp='git push'

alias py='python3'

alias l='eza -alF --icons --group-directories-first'
alias li='l --git-ignore'

generate_todo_alias() {
    local cmd="tgrep '\\bTODO\\b'"
    local to_add=("CHECK" "FIX" "NOTE" "CLEANUP")
    for keyword in "${to_add[@]}"; do
        cmd+=" --regexp '\\b${keyword}\\b'"
    done
    cmd+=" --searcher=treegrep"
    alias todo="$cmd"
}
generate_todo_alias
