for FILE in ~/loaded_scripts/*; do
    source $FILE
done

#boot into tmux
if [ -z "$TMUX" ]
then
   tmux attach -t def || tmux new -s def
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'
export FZF_DEFAULT_COMMAND="fd --type f"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND";
