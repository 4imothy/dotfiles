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

if [ $SUGGESTIONS -eq 1 ]; then
    bindkey '^n' autosuggest-accept
    ZSH_AUTOSUGGEST_STRATEGY=(completion)

    url="https://raw.githubusercontent.com/zsh-users/zsh-autosuggestions/master/zsh-autosuggestions.zsh"
    dir="$ZDOTDIR/external"
    file=$dir/zsh-autosuggestions.zsh
    ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=white'

    if [ ! -d "$dir" ]; then
        mkdir -p "$dir"
    fi
    if [ ! -f "$file" ]; then
        curl -o "$file" "$url"
    fi
    source $file
fi
