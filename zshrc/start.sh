# boot into tmux
# if [ "$TMUX" = "" ]; then tmux; fi
# if [ -z "$TMUX" ]; then
#   tmux attach || tmux
#fi
if [ -z "$TMUX" ]
then
    tmux attach -t TMUX || tmux new -s TMUX
fi

