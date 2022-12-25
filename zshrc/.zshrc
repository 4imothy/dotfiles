for FILE in ~/loaded_scripts/*; do
    source $FILE
done

# boot into tmux
if [ -z "$TMUX" ]
    then
	 tmux attach -t def || tmux new -s def
fi
