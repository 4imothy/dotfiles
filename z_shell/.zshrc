export precmd_functions=()

for FILE in ~/loaded_scripts/*; do
    source $FILE
done

if [[ "$colors" == "dark" ]]; then
    echo "In dark mode"
    for FILE in ~/loaded_scripts/dark_mode/*; do
	source $FILE
    done
else
    echo "In light mode"
    for FILE in ~/loaded_scripts/light_mode/*; do
	source $FILE
    done
fi


# enter default tmux server on start
if [ -z "$TMUX" ]
    then
	 tmux attach -t def || tmux new -s def
fi
