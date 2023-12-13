# cursor switching for vim
# Remove mode switching delay.
export EDITOR='vim'
bindkey "^?" backward-delete-char

KEYTIMEOUT=5
setopt transientrprompt

vim_ins_mode="%F{7}❮%f%F{7}INS%f%F{7}❯%f"
vim_cmd_mode="%F{7}❮%f%F{7}NOR%f%F{7}❯%f"

zle-line-init() {
set_rp
zle reset-prompt
}

set_rp() {
    RPS1="${${KEYMAP/vicmd/${vim_cmd_mode}}/(main|viins)/${vim_ins_mode}}"
    RPS2=$RPS1
}

vi_update_cursor_color() {
    if [[ ${KEYMAP} == vicmd ]] || [[ $1 = 'block' ]]; then
        echo -ne $SET_NORMAL_CURSOR_COLOR
    elif [[ ${KEYMAP} == main ]] || [[ ${KEYMAP} == viins ]] ||
        [[ ${KEYMAP} = '' ]] || [[ $1 = 'beam' ]]; then
            echo -ne $SET_DEFAULT_CURSOR_COLOR
    fi
}

vi_update_cursor_shape() {
    if [[ ${KEYMAP} == vicmd ]] || [[ $1 = 'block' ]]; then
        echo -ne $SET_BLOCK_CURSOR
    elif [[ ${KEYMAP} == main ]] || [[ ${KEYMAP} == viins ]] ||
        [[ ${KEYMAP} = '' ]] || [[ $1 = 'beam' ]]; then
            echo -ne $SET_BEAM_CURSOR
    fi
}

zle-keymap-select() {
if [ $VI_CHANGE_CURSOR_SHAPE -eq 1 ]; then
    vi_update_cursor_shape "$@"
fi
if [ $VI_CHANGE_CURSOR_COLOR -eq 1 ]; then
    vi_update_cursor_color "$@"
fi
set_rp
zle reset-prompt
}

zle -N zle-keymap-select
zle -N zle-line-init
set_rp

reset_cursor_shape() {
    echo -ne '\e[6 q'
}

if [ $VI_CHANGE_CURSOR_SHAPE -eq 1 ]; then
    add_to_preexec "reset_cursor_shape"
    add_to_precmd "reset_cursor_shape"
fi
