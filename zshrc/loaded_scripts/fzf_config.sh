[ -f ~/loaded_scripts/fzf-zsh-plugin/fzf-zsh-plugin.plugin.zsh ] && source ~/loaded_scripts/fzf-zsh-plugin/fzf-zsh-plugin.plugin.zsh
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'
export FZF_DEFAULT_COMMAND="fd --type f"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND";

# find and go to dir
d() {
  DIR=`find * -maxdepth 0 -type d -print 2> /dev/null | fzf-tmux ` \
    && cd "$DIR"
}

# get something in this directory (not called f so it doesn't overide fzf)
g() {
    sels=( "${(@f)$(fd "${fd_default[@]}" "${@:2}"| fzf)}" )
   test -n "$sels" && print -z -- "$1 ${sels[@]:q:q}"
}

# navigate through the current directory to directories (depth 1)
nd(){
    cd $( ls | fzf)
}

# search for a directory then go to it (no limit on depth)
sd(){
  local dir
  dir=$(find ${1:-.} . \( -path '*/Library/*' -prune -o -path '*/node_modules/*' -prune -o -path '*/Pictures/*' -prune -o -path '*/.*' -prune \) -o -type d | fzf ) && cd "$dir"
}

# fda - including hidden directories
find_dir_a() {
  local dir
  dir=$(find ${1:-.} -type d 2> /dev/null | fzf +m) && cd "$dir"
}
