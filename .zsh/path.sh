path_dirs=("$HOME/bin" "$HOME/.cargo/bin" "$HOME/.go/bin"
           "/opt/homebrew/opt/qt5/bin" "$ZDOTDIR/scripts")

for dir in "${path_dirs[@]}"; do
    if [[ ":$PATH:" != *":$dir:"* ]]; then
        export PATH="$dir:$PATH"
    fi
done

export GOPATH="$HOME/.go"

export JAVA_HOME="$(/usr/libexec/java_home)"
export CPPFLAGS="-I/opt/homebrew/opt/openjdk/include"
