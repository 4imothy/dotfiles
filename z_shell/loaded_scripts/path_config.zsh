local FZF_PATH=~/.fzf
if [[ ! "$PATH" == *${FZF_PATH}/bin* ]]; then
  export PATH="$PATH:${FZF_PATH}/bin"
fi
if [[ ! "$PATH" == *$HOME/bin* ]]; then
  export PATH="$PATH:$HOME/bin"
fi
# for qmake
if [[ ! "$PATH" == */opt/homebrew/opt/qt@5/bin* ]]; then
  export PATH="$PATH:/opt/homebrew/opt/qt@5/bin"
fi
