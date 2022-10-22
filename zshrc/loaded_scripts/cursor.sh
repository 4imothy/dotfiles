# cursor switching for vim
# Remove mode switching delay.
KEYTIMEOUT=5

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
    }
resetcursor() {
    # use beam on new prompt
    echo -ne '\e[5 q'
}
precmd_functions+=( resetcursor )
zle -N zle-keymap-select
