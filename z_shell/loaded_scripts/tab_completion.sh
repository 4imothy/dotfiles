# initialize the completion
autoload -U compinit; compinit
zstyle ':completion:*' completer _extensions _complete _approximate group-name ''
zstyle ':completion:*' menu yes select
zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'
zstyle ':completion:*:default' list-colors ${LS_COLORS}
zstyle ':completion:*' group-name ''
zstyle ':completion:*:*:*:*:descriptions' format '%F{135}-- %d --%f'
zstyle '*' single-ignored complete

setopt ALWAYS_TO_END
