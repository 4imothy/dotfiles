export HISTFILE="$HOME/.zsh_history"
export EDITOR=nvim
export SHOW_BRANCH_IN_PROMPT=0
export SHOW_BRANCH_IN_RPROMPT=1
export PAR_DIR_PRINT_LIMIT=2
export CLICOLOR=1
export LSCOLORS=fxfxcxdxbxegedabagacfx

export VI_CHANGE_CURSOR_SHAPE=1
export VI_CHANGE_CURSOR_COLOR=0
export SUGGESTIONS=1
export TODO_FILE="~/Documents/notes/index.norg"

bindkey -v
setopt SHARE_HISTORY

add_to_array() {
   if [[ ! " $1 " =~ " $2" ]]; then
        eval "$1+=(\"$2\")"
    fi
}

for file in "$ZDOTDIR"/*.sh "$ZDOTDIR"/*.zsh; do
    source "$file"
done

if [ -z $TMUX ]; then
    ses --popup
fi
