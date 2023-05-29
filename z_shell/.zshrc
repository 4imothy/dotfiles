# so that the correct update_prompt function is always added no matter the color scheme
export precmd_functions=()

# load all the files in the directory with depth 1
for FILE in ~/sourced/*; do
    source $FILE
done

for FILE in ~/sourced/completions/*; do
    source $FILE
done
