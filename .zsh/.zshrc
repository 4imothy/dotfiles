export HISTFILE="$HOME/.zsh_history"
export EDITOR='vim'
export SHOW_BRANCH_IN_PROMPT=1
export PAR_DIR_PRINT_LIMIT=2
export CLICOLOR=1
export LSCOLORS=fxfxcxdxbxegedabagacfx

# TODO configure ability to show git information in the RPROMPT
export VI_CHANGE_CURSOR_SHAPE=1
export VI_CHANGE_CURSOR_COLOR=1
export DEFAULT_CURSOR_COLOR=#d3c6aa
export NORMAL_CURSOR_COLOR=#83c092

if [ $VI_CHANGE_CURSOR_SHAPE -eq 1 ]; then
    export SET_NORMAL_CURSOR="\e[2 q"
    export SET_INSERT_CURSOR="\e[6 q"
fi
if [ $VI_CHANGE_CURSOR_COLOR -eq 1 ]; then
    export SET_NORMAL_CURSOR="${SET_NORMAL_CURSOR}\e]12;$NORMAL_CURSOR_COLOR\x7"
    export SET_INSERT_CURSOR="${SET_INSERT_CURSOR}\e]12;$DEFAULT_CURSOR_COLOR\x7"
fi

export SET_RPROMPT=0

bindkey -v
setopt SHARE_HISTORY

add_to_array() {
   if [[ ! " $1 " =~ " $2" ]]; then
        eval "$1+=(\"$2\")"
    fi
}

for file in "$ZDOTDIR"/*.sh; do
    source "$file"
done
