# cursor switching for vim
# Remove mode switching delay.
bindkey "^?" backward-delete-char

KEYTIMEOUT=5
export DEFAULT_CURSOR_COLOR=#d3c6aa
export NORMAL_CURSOR_COLOR=#83c092

if [ $VI_CHANGE_CURSOR_SHAPE -eq 1 ]; then
    export SET_NORMAL_CURSOR="\e[2 q"
    export SET_INSERT_CURSOR="\e[4 q"
fi
if [ $VI_CHANGE_CURSOR_COLOR -eq 1 ]; then
    export SET_NORMAL_CURSOR="${SET_NORMAL_CURSOR}\e]12;$NORMAL_CURSOR_COLOR\x7"
    export SET_INSERT_CURSOR="${SET_INSERT_CURSOR}\e]12;$DEFAULT_CURSOR_COLOR\x7"
fi

zle-line-init() {
zle reset-prompt
}

vi_update_cursor() {
    if [[ ${KEYMAP} == vicmd ]]; then
        echo -ne $SET_NORMAL_CURSOR
    elif [[ ${KEYMAP} == main ]] || [[ ${KEYMAP} == viins ]] ||
        [[ ${KEYMAP} = '' ]]; then
            echo -ne $SET_INSERT_CURSOR
    fi
}

zle-keymap-select() {
if [[ $VI_CHANGE_CURSOR_SHAPE -eq 1 ]] || [[ $VI_CHANGE_CURSOR_COLOR -eq 1 ]]; then
    vi_update_cursor "$@"
fi
zle reset-prompt
}

zle -N zle-keymap-select
reset_cursor() {
    echo -ne $SET_INSERT_CURSOR
}

if [ $VI_CHANGE_CURSOR_SHAPE -eq 1 ]; then
    add_to_array preexec_functions "reset_cursor"
    add_to_array precmd_functions "reset_cursor"
fi
