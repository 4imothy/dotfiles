[ -f ~/loaded_scripts/fzf-zsh-plugin/fzf-zsh-plugin.plugin.zsh ] && source ~/loaded_scripts/fzf-zsh-plugin/fzf-zsh-plugin.plugin.zsh
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'
export FZF_DEFAULT_COMMAND="fd --type f"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND";

d() {
  DIR=`find * -maxdepth 0 -type d -print 2> /dev/null | fzf-tmux ` \
    && cd "$DIR"
}
# fda - including hidden directories
find_dir_a() {
  local dir
  dir=$(find ${1:-.} -type d 2> /dev/null | fzf +m) && cd "$dir"
}

# navigate through the current directory
nd(){
    cd $( ls | fzf)
}

search_for_dir(){
    cd $( find . \( -path '*/Library/*' -prune -o -path '*/node_modules/*' -prune -o -path '*/Pictures/*' -prune -o -path '*/.*' -prune \) -o type -d | fzf )
    # dir = `find . \( -path '*/Library/*' -prune -o -path '*/node_modules/*' -prune -o -path '*/Pictures/*' -prune -o -path '*/.*' -prune \) -o type -d | fzf ` \
    # && cd "$dir"
}
