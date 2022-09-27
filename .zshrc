for FILE in ~/zshrc/*; do
    source $FILE
done

#boot into tmux
# if [ -z "$TMUX" ]
# then
#    tmux attach -t TMUX || tmux new -s TMUX
# fi
