# cursor switching for vim
# Remove mode switching delay.
KEYTIMEOUT=5
setopt transientrprompt

# Change cursor shape for different vi modes.
function zle-keymap-select {
    if [[ ${KEYMAP} == vicmd ]] ||
	[[ $1 = 'block' ]]; then
	    echo -ne '\e[1 q'
    elif [[ ${KEYMAP} == main ]] ||
	[[ ${KEYMAP} == viins ]] ||
	[[ ${KEYMAP} = '' ]] ||
	[[ $1 = 'beam' ]]; then
	    echo -ne '\e[5 q'
	fi
    RPS1="${${KEYMAP/vicmd/${vim_cmd_mode}}/(main|viins)/${vim_ins_mode}}"
    RPS2=$RPS1
    zle reset-prompt
}
autoload color
# vim_ins_mode="%{$fg[white]%}%{$fg_bold[blue]$bg[white]%} INSERT %{$reset_color%}"
# vim_cmd_mode="%{$fg[green]%}%{$fg_bold[black]$bg[green]%} COMMAND %{$reset_color%}"
vim_ins_mode=" INSERT "
vim_cmd_mode=" COMMAND "
vim_mode=$vim_ins_mode
function zle-line-init {
    RPS1="${${KEYMAP/vicmd/${vim_cmd_mode}}/(main|viins)/${vim_ins_mode}}"
    RPS2=$RPS1
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

resetcursor() {
    # use beam on new prompt
    echo -ne '\e[5 q'
}
precmd_functions+=( resetcursor )
