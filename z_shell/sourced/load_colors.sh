if [[ "$colors" == "bubblegum" ]]; then
    echo "Bubblegum Colors"
    for FILE in ~/sourced/bubblegum/*; do
	source $FILE
    done
else
    echo "Solarized Colors"
    for FILE in ~/sourced/solarized/*; do
	source $FILE
    done
fi
