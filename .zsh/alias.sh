alias c="clear"
alias ch="history -p; clear"
alias ct='clear; history -p; clear; tmux clear-history; clear'
alias todo="tgrep TODO"
alias tg="tgrep"
alias todo="tgrep --regexp TODO --regexp CHECK --regexp FIX --regexp NOTE --regexp CLEANUP --searcher=treegrep"
alias venv="source venv/bin/activate"
alias todom="todo -m"
alias tree="tgrep --tree"
alias ltree="tgrep --tree --long-branch"
alias run="python3 run.py"

alias gs='git status'
alias ga='git add'
alias gc='git commit'
alias gp='git push'

alias py='python3'

alias l='eza -alF --icons --group-directories-first'
alias li='l --git-ignore'

generate_todo_alias() {
    local keywords=("TODO" "CHECK" "FIX" "NOTE" "CLEANUP")
    local cmd="tgrep"
    for keyword in "${keywords[@]}"; do
        cmd+=" --regexp '\\b${keyword}\\b'"
    done
    cmd+=" --searcher=treegrep"
    alias todo="$cmd"
}
generate_todo_alias
