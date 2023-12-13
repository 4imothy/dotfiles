export HISTFILE="$HOME/.zsh_history"
export EDITOR='vim'
export SHOW_BRANCH_IN_PROMPT=1
export PAR_DIR_PRINT_LIMIT=2
export CLICOLOR=1
export LSCOLORS=fxfxcxdxbxegedabagacfx

# TODO this should instead by done with two escape sequences
# one for insert and one for normal, start with empty and then
# add on the things that are needed, this should need less
# functions in pre(cmd/exec) and less stuff in cursor.vim
export VI_CHANGE_CURSOR_SHAPE=1
export VI_CHANGE_CURSOR_COLOR=1
export DEFAULT_CURSOR_COLOR=#d3c6aa
export NORMAL_CURSOR_COLOR=#83c092
export SET_DEFAULT_CURSOR_COLOR="\e]12;$DEFAULT_CURSOR_COLOR\x7"
export SET_NORMAL_CURSOR_COLOR="\e]12;$NORMAL_CURSOR_COLOR\x7"
export SET_BLOCK_CURSOR="\e[2 q"
export SET_BEAM_CURSOR="\e[6 q"
export SET_RPROMPT=0

bindkey -v
setopt SHARE_HISTORY

add_to_precmd() {
    if [[ "$precmd_functions" != *"$1"* ]]; then
        precmd_functions+=("$1")
    fi
}

add_to_preexec() {
    if [[ "$preexec_functions" != *"$1"* ]]; then
        preexec_functions+=("$1")
    fi
}

for file in "$ZDOTDIR"/*.sh; do
    source "$file"
done
