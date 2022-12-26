for FILE in ~/loaded_scripts/*; do
    source $FILE
done

# enter default tmux server on start
if [ -z "$TMUX" ]
    then
	 tmux attach -t def || tmux new -s def
fi
