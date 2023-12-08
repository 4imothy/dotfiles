# cursor switching for vim
# Remove mode switching delay.
export EDITOR='vim'
bindkey "^?" backward-delete-char

KEYTIMEOUT=5
setopt transientrprompt

vim_ins_mode="%F{7}❮%f%F{7}INS%f%F{7}❯%f"
vim_cmd_mode="%F{7}❮%f%F{7}NOR%f%F{7}❯%f"
function zle-line-init {
    setRP
    zle reset-prompt
}
function setRP {
    RPS1="${${KEYMAP/vicmd/${vim_cmd_mode}}/(main|viins)/${vim_ins_mode}}"
    RPS2=$RPS1
}

function zle-keymap-select {
    # if [[ ${KEYMAP} == vicmd ]] || [[ $1 = 'block' ]]; then
    #     echo -ne '\e[2 q'
    # elif [[ ${KEYMAP} == main ]] ||
    #  [[ ${KEYMAP} == viins ]] ||
    #  [[ ${KEYMAP} = '' ]] ||
    #  [[ $1 = 'beam' ]]; then
    #      echo -ne '\e[6 q'
    # fi
    setRP
    zle reset-prompt
}

zle -N zle-keymap-select
zle -N zle-line-init
setRP

reset_cursor() {
    # use beam on new prompt
    echo -ne '\e[6 q'
}
# use block always
if [[ ! "$precmd_functions" == *reset_cursor* ]]; then
#    precmd_functions+=(reset_cursor)
fi
