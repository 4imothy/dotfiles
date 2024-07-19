alias c="clear"
alias ch="history -p; clear"
alias ct='clear; history -p; clear; tmux clear-history; clear'
alias todo="tgrep TODO"
alias tg="tgrep"
alias todo="tgrep --regexp TODO --regexp FIXME --searcher=treegrep"
alias venv="source venv/bin/activate"
alias todom="todo -m"
alias tree="tgrep --tree"

alias gs='git status'
alias ga='git add'
alias gc='git commit'
alias gp='git push'

alias py='python3'

alias ls='eza --icons --color=always --group-directories-first'
alias ll='eza -alF --icons --color=always --group-directories-first'
alias la='eza -a --icons --color=always --group-directories-first'
alias l='eza -F --icons --color=always --group-directories-first'
