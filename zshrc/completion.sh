autoload -U compinit; compinit
zstyle ':completion:*' completer _extensions _complete _approximate group-name ''
zstyle ':completion:*' menu select
zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'
zstyle ':completion:*:default' list-colors ${LS_COLORS}

zstyle ':completion:*:*:*:*:descriptions' format '%F{135}-- %d --%f'

setopt ALWAYS_TO_END
