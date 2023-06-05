# for scripts i made
if [[ ! "$PATH" == *$HOME/bin* ]]; then
  export PATH="$PATH:$HOME/bin"
fi

# for qmake
if [[ ! "$PATH" == */opt/homebrew/opt/qt5/bin* ]]; then
  export PATH="$PATH:/opt/homebrew/opt/qt5/bin"
fi

# to initialize using wgsl, and other cargo installed bins
start_wgsl() { 
  if [[ ! "$PATH" == *$HOME/.cargo/bin* ]]; then
    source $HOME/.cargo/env
  fi
}

# to initialize for rust development
start_rust() {
  if [[ ! "$PATH" == *$HOME/.cargo/bin* ]]; then
    source $HOME/.cargo/env
  fi
  if [[ ! "$PATH" == *$HOME/.rustup/toolchains/stable-aarch64-apple-darwin/bin* ]]; then
    export PATH="$HOME/.rustup/toolchains/stable-aarch64-apple-darwin/bin:$PATH"
  fi
}

# to initialize go development
start_go() {
  if [[ ! "$PATH" == *$HOME/go/bin* ]]; then
    export PATH="$HOME/go/bin:$PATH"
  fi
}

export JAVA_HOME="$(/usr/libexec/java_home)"
# this line was necessary for python-bioformats
export CPPFLAGS="-I/opt/homebrew/opt/openjdk/include"
