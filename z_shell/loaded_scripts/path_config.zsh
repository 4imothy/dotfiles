if [[ ! "$PATH" == *$HOME/bin* ]]; then
  export PATH="$PATH:$HOME/bin"
fi
if [[ ! "$PATH" == *$HOME/.fzf/bin* ]]; then
  export PATH="$PATH:$HOME/.fzf/bin"
fi
# for qmake
if [[ ! "$PATH" == */opt/homebrew/opt/qt@5/bin* ]]; then
  export PATH="$PATH:/opt/homebrew/opt/qt@5/bin"
fi
