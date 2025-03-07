mcd() {
    test -d "$1" || mkdir "$1" && cd "$1"
}

batt() {
    pmset -g batt | grep -E "([0-9]+\%).*" -o --colour=auto | cut -f1 -d';'
}

brew_deps() {
    brew deps --tree --installed $1
}

deflatex() {
    if [ -z "$1" ]; then
        latexmk -pvc -pdf --interaction=nonstopmode *.tex
    else
        if [[ ! "$1" =~ \.tex$ ]]; then
            filename="$1.tex"
        else
            filename="$1"
        fi
        if [ -z "$2" ]; then
            latexmk -pvc -pdf --interaction=nonstopmode "$filename"
        else
            latexmk -pvc -pdf --interaction=nonstopmode -outdir="$2" "$filename"
        fi
    fi
}

deftypst() {
    if [ -z "$1" ]; then
        typst watch *.typ --open
    else
        if [[ ! "$1" =~ \.typ$ ]]; then
            filename="$1.typ"
        else
            filename="$1"
        fi
        typst watch $filename --open
    fi
}

sddg() {
    query="$*"
    open "https://www.duckduckgo.com/?q=${query}"
}

sgh() {
    query="$*"
    open "https://github.com/search?q=${query}"
}

syt() {
    query="$*"
    open "https://www.youtube.com/results?search_query=$query{}"
}

swk() {
    query="$*"
    open "https://en.wikipedia.org/wiki/${query}"
}
