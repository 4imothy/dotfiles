. "$HOME/.cargo/env"

case ":${PATH}:" in
    *:"$HOME/bin":*)
        ;;
    *)
        export PATH="$HOME/bin:$PATH"
        ;;
esac
