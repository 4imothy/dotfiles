if [[ "$colors" == "bubblegum" ]]; then
    for FILE in ~/sourced/bubblegum/*; do
	source $FILE
    done
else
    for FILE in ~/sourced/solarized/*; do
	source $FILE
    done
fi
