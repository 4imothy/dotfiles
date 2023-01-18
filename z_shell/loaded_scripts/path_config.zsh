# for scripts i made
if [[ ! "$PATH" == *$HOME/bin* ]]; then
  export PATH="$PATH:$HOME/bin"
fi

# for qmake
if [[ ! "$PATH" == */opt/homebrew/opt/qt5/bin* ]]; then
  export PATH="$PATH:/opt/homebrew/opt/qt5/bin"
fi

# to initialize for rust development
start_rust() { 
  if [[ ! "$PATH" == *$HOME/.cargo/bin* ]]; then
    source $HOME/.cargo/env
  fi 
  if [[ ! "$PATH" == *$HOME/.rustup/toolchains/stable-aarch64-apple-darwin/bin* ]]; then
    export PATH="$HOME/.rustup/toolchains/stable-aarch64-apple-darwin/bin:$PATH"
  fi
}  