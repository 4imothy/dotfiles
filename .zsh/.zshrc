export HISTFILE="$HOME/.zsh_history"
export EDITOR='vim'
export SHOW_BRANCH_IN_PROMPT=1
export PAR_DIR_PRINT_LIMIT=2
export VI_CHANGE_CURSOR_SHAPE=1
export VI_CHANGE_CURSOR_COLOR=1
export DEFAULT_CURSOR_COLOR=#d3c6aa
export NORMAL_CURSOR_COLOR=#83c092
export CLICOLOR=1
export LSCOLORS=fxfxcxdxbxegedabagacfx

bindkey -v
setopt SHARE_HISTORY

add_function_to_precmd() {
    if [[ "$precmd_functions" != *"$1"* ]]; then
        precmd_functions+=("$1")
    fi
}

add_function_to_preexec() {
    if [[ "$preexec_functions" != *"$1"* ]]; then
        preexec_functions+=("$1")
    fi
}

for file in "$ZDOTDIR"/*.sh; do
    source "$file"
done
