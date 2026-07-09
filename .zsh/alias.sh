alias c="clear"
alias ch="history -p; clear"
alias ct='clear; history -p; clear; tmux clear-history; clear'
alias tg="tgrep"
alias tgs="tg --select"
alias venv="source venv/bin/activate"
alias todo="tg -e TODO -e CHECK -e FIX -e NOTE -e CLEANUP"
alias todos="todo -s"
alias tree="tg --files"
alias ltree="tg --files --branch-each 5"
alias run="python3 run.py"
alias reload="source $ZDOTDIR/.zshrc"
alias fmtclip="cargo +nightly fmt && cargo +nightly clippy --all-features"

alias gs='git status'
alias ga='git add'
alias gc='git commit'
alias gp='git push'

alias py='python3'

alias l='eza -alF --icons --group-directories-first'
alias li='l --git-ignore'
