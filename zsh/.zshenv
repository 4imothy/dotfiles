. "$HOME/.cargo/env"

case ":${PATH}:" in
    *:"$HOME/bin":*)
        ;;
    *)
        export PATH="$HOME/bin:$PATH"
        ;;
esac

case ":${PATH}:" in
    *:"$HOME/.go/bin":*)
        ;;
    *)
        export PATH="$HOME/.go/bin:$PATH"
        ;;
esac

export GOPATH="$HOME/.go"
