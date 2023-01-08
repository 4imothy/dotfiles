# for scripts
if [[ ! "$PATH" == *$HOME/bin* ]]; then
  export PATH="$PATH:$HOME/bin"
fi
# for qmake
if [[ ! "$PATH" == */opt/homebrew/opt/qt5/bin* ]]; then
  export PATH="$PATH:/opt/homebrew/opt/qt5/bin"
fi
