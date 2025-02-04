export HISTFILE="$HOME/.zsh_history"
export EDITOR=nvim
export NOTES_EDITOR=emacs
export SHOW_BRANCH_IN_PROMPT=0
export SHOW_BRANCH_IN_RPROMPT=1
export PAR_DIR_PRINT_LIMIT=2
export CLICOLOR=1
export LSCOLORS=fxfxcxdxbxegedabagacfx

export ACPP_DEBUG_LEVEL=1
export ACPP_OMP_CXX_FLAGS="\
    -I/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/c++/v1 \
    -I/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include \
    -I/opt/homebrew/opt/libomp/include \
    -I/opt/homebrew/opt/boost/include \
    -I/Users/timothy/Projects/ai3/venv/include"

export ACPP_OMP_LINK_LINE="\
    -L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib \
    -L/Library/Developer/CommandLineTools/usr/lib \
    -L/opt/homebrew/opt/libomp/lib \
    -L/opt/homebrew/opt/boost/lib \
    -lomp -lboost_system-mt -lboost_context-mt -lboost_fiber-mt"


export VI_CHANGE_CURSOR_SHAPE=1
export VI_CHANGE_CURSOR_COLOR=0
export SUGGESTIONS=1
export TODO_FILE="$HOME/Documents/org/tasks.org"
export TREEGREP_DEFAULT_OPTS="--glob=!.git --hidden --char-style=rounded"

bindkey -v
setopt SHARE_HISTORY

add_to_array() {
   if [[ ! " $1 " =~ " $2" ]]; then
        eval "$1+=(\"$2\")"
    fi
}

add_to_array XDG_CONFIG_HOME "$HOME/.config"

for file in "$ZDOTDIR"/*.sh "$ZDOTDIR"/*.zsh; do
    source "$file"
done

# if [ -z $ZELLIJ ]; then
#     zellij -l welcome
# fi

tfile() {
    local first_arg="$1"
    shift
    tgrep --tree --glob="*$first_arg*" "$@"
}

if [ -z $TMUX ]; then
    ses --popup
fi
