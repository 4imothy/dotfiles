# so that the correct update_prompt function is always added no matter the color scheme
export precmd_functions=()

for FILE in ~/loaded_scripts/*; do
    source $FILE
done

if [[ "$colors" == "bubblegum" ]]; then
    echo "Bubblegum Colors"
    for FILE in ~/loaded_scripts/bubblegum/*; do
	source $FILE
    done
else
    echo "Solarized Colors"
    for FILE in ~/loaded_scripts/solarized/*; do
	source $FILE
    done
fi


# enter default tmux server on start
if [ -z "$TMUX" ]
    then
	 tmux attach -t def || tmux new -s def
fi
