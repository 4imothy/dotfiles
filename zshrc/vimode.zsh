# cursor switching for vim
# Remove mode switching delay.
KEYTIMEOUT=5
setopt transientrprompt

# Change cursor shape for different vi modes.
function zle-keymap-select {
    if [[ ${KEYMAP} == vicmd ]] ||
	[[ $1 = 'block' ]]; then
	    echo -ne '\e[2 q'
    elif [[ ${KEYMAP} == main ]] ||
	[[ ${KEYMAP} == viins ]] ||
	[[ ${KEYMAP} = '' ]] ||
	[[ $1 = 'beam' ]]; then
	    echo -ne '\e[6 q'
	fi
    setRP
    zle reset-prompt
}
vim_ins_mode="%F{037}❮%f%F{140}INSERT%f%F{037}❯%f"
vim_cmd_mode="%F{037}❮%f%F{140}COMMAND%f%F{037}❯%f"
# vim_ins_mode=" INSERT "
# vim_cmd_mode=" COMMAND "
vim_mode=$vim_ins_mode
function zle-line-init {
    setRP
    zle reset-prompt
}
function setRP {
    RPS1="${${KEYMAP/vicmd/${vim_cmd_mode}}/(main|viins)/${vim_ins_mode}}"
    RPS2=$RPS1
}
zle -N zle-keymap-select
zle -N zle-line-init
setRP

resetcursor() {
    # use beam on new prompt
    echo -ne '\e[6 q'
}
precmd_functions+=( resetcursor )
