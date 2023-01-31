listening_ports() {
  sudo lsof -P -i -n | grep LISTEN
}

copytodotfiles(){
    cp -r ~/sourced ~/Projects/seperate_git_repos/dotfiles/z_shell/
    cp -r ~/.qutebrowser/ ~/Projects/seperate_git_repos/dotfiles/browsers/qutebrowser
    cp -r ~/.config/helix ~/Projects/seperate_git_repos/dotfiles/
    cp ~/Library/Application\ Support/Firefox/Profiles/ry1zhwm6.default-release/chrome/userChrome.css ~/Projects/seperate_git_repos/dotfiles/browsers/firefox/
    cp ~/.zshrc ~/Projects/seperate_git_repos/dotfiles/z_shell
    cp ~/.config/alacritty/alacritty.yml ~/Projects/seperate_git_repos/dotfiles/terminals/
    cp ~/.tmux.conf ~/Projects/seperate_git_repos/dotfiles/tmux
    cp ~/.hushlogin ~/Projects/seperate_git_repos/dotfiles/z_shell
    cp -Rf ~/.config/nvim ~/Projects/seperate_git_repos/dotfiles
    cp -Rf ~/Documents/Vault/.obsidian/snippets/ ~/Projects/seperate_git_repos/dotfiles/obsidian
}

change_colors(){
    export colors=$1
    source ~/.zshrc
}

# make a directory and cd to it
mcd()
{
    test -d "$1" || mkdir "$1" && cd "$1"
}

batt() 
{
pmset -g batt | grep -E "([0-9]+\%).*" -o --colour=auto | cut -f1 -d';'
} 

modified_bin() {
    ls -tl /bin | head -2 | tail +2 | awk '{print "/bin last modified: " substr($0,30)}'
    ls -tl /bin | awk '{sum+=$5}END{print "total bytes in /bin = " sum}'
    ls -tl /usr/bin | head -2 | tail +2 | awk '{print "/usr/bin last modified: " substr($0,30)}'
    ls -tl /usr/bin | awk '{sum+=$5}END{print "total bytes in /usr/bin = " sum}'
}

brew_deps() {
    brew deps --tree --installed
}

# search duck duck go
sddg() {
    query="$*" 
    open "https://www.duckduckgo.com/?q=${query}"
}

# search git hub
sgh() {
    query="$*"
    open "https://github.com/search?q=${query}"
} 

# search youtube
syt() {
    query="$*"
    open "https://www.youtube.com/results?search_query=$query{}"
}

# search wikipedia
swk() {
    query="$*"
    open "https://en.wikipedia.org/wiki/${query}"
}
