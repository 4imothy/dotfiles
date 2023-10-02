# . "$HOME/.cargo/env"

# Directories to add to PATH
path_dirs=("$HOME/bin" "$HOME/.cargo/bin" "$HOME/.go/bin")

for dir in "${path_dirs[@]}"; do
    if [[ ":$PATH:" != *":$dir:"* ]]; then
        export PATH="$dir:$PATH"
    fi
done

export GOPATH="$HOME/.go"
alias todo="gret TODO"
