# so that the correct update_prompt function is always added no matter the color scheme
export precmd_functions=()

# load all the files in the directory with depth 1
for FILE in ~/sourced/*; do
    source $FILE
done
 
# enter default tmux server on start
if [ -z "$TMUX" ]
    then
	 tmux attach -t def || tmux new -s def
fi
