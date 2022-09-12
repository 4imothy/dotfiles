export EDITOR='nvim'
bindkey -v
bindkey "^?" backward-delete-char
setopt autocd

NEWLINE=$'\n'

# teal colors
# firstLine="%F{049}╭─%/%f"
# secondLine="%F{043}╰─❱%f%F{037}❱%f%F{031}❱%f %F{111}"

# lavender with red yellow green
# firstLine="%F{105}╭─%/%f%k"
# secondLine="%F{104}╰─%f%F{9}❱%f%F{11}❱%f%F{10}❱%f%F{123} "

# grayscale
# firstLine="%F{242}╭─%/%f%k"
# secondLine="%F{243}╰─%f%F{244}❱%f%F{245}❱%f%F{246}❱%f%F{248}

#  
# firstLine="%F{214}╭─%f%F{076}%/%f%k"
# secondLine="%F{214}╰─%f%F{166}❱%f%F{167}❱%f%F{168}❱%f%F{191}"

# sunset
firstLine="%F{135}╭─%f%F{075}%/%f%k"
secondLine="%F{169}╰─%f%F{168}❱%f%F{167}❱%f%F{166}❱%f%F{173}"

PROMPT="${firstLine}${NEWLINE}${secondLine} "

export CLICOLOR=1
export LSCOLORS=fxfxcxdxbxegedabagacfx

alias vim='nvim'
alias vi='nvim'

alias galgo='cd ~/School/Algorithms/'
alias gline='cd ~/School/Linear'
alias glogi='cd ~/School/LogicDesign'
alias gmicr='cd ~/School/Micro/'
alias gcalc='cd ~/School/Calc3/'
alias zshrc='vi ~/.zshrc'
alias ghome='cd ~/'
alias groot='cd /'
alias pu='pushd'
alias po='popd'
alias ll='ls -l'
alias la='ls -a'

# open common apps
alias obs='open /Applications/Obsidian.app'
alias fir='open /Applications/Firefox.app'
alias spo='open /Applications/Spotify.app'
alias mai='open /System/Applications/Mail.app'
alias mes='open /System/Applications/Messages.app'

# git for dotfiles repo
pushDotfiles() {
    cp /Users/tcron/.zshrc /Users/tcron/.config/dotfiles
    cp -Rf /Users/tcron/.config/nvim /Users/tcron/.config/dotfiles
    cp -Rf ~/School/Notes/.obsidian/snippets/ ~/.config/dotfiles/obsidian
    git -C /Users/tcron/.config/dotfiles add .
    git -C /Users/tcron/.config/dotfiles commit -m $1
    git -C /Users/tcron/.config/dotfiles push -u origin main
}

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
zle -N zle-keymap-select

    # Use beam shape cursor on startup.
    echo -ne '\e[5 q'

    preexec() {
	# Use beam shape cursor for each new prompt.
	echo -ne '\e[5 q'
	# change back to normal color 
	print -Pn "%f" 
    }
    
