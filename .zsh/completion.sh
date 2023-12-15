my_completions="$ZDOTDIR/completions"
add_to_array fpath $my_completions

autoload -U compinit
compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION
zstyle ':completion:*' completer _extensions _complete _approximate
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu yes select
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'
zstyle ':completion:*:default' list-colors ${LS_COLORS}
zstyle ':completion:*:*:*:*:descriptions' format '%F{magenta}-- %d --%f'
zstyle '*' single-ignored complete
setopt ALWAYS_TO_END
