alias vi='nvim'
alias siftop='sudo iftop -i en0'
alias c="clear"
alias ch="history -p; clear"
alias ct='clear; history -p; clear; tmux clear-history; clear' # tmux clear

# git
alias gs='git status'
alias ga='git add'
alias gc='git commit'
alias gp='git push'

alias py='python3'

# tmux
alias switch="tmux switch -t"

# directory info
alias l='ls'
alias ll='ls -l'
alias la='ls -a'
alias wds='du -sh' # working directory size

alias reload="source ~/.zshrc"

alias -s {js,ts,html,java,md,py}="nvim"

# other functions
alias amtp="add_math_scripts_to_path"
