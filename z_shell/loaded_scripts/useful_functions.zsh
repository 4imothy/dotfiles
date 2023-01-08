listening_ports() {
  sudo lsof -P -i -n | grep LISTEN
}

copytodotfiles(){
    cp -r ~/loaded_scripts ~/Projects/seperate_git_repos/dotfiles/z_shell/
    cp -r ~/.qutebrowser/ ~/Projects/seperate_git_repos/dotfiles/browsers/qutebrowser
    cp ~/Library/Application\ Support/Firefox/Profiles/ry1zhwm6.default-release/chrome/userChrome.css ~/Projects/seperate_git_repos/dotfiles/browsers/firefox/
    cp ~/.zshrc ~/Projects/seperate_git_repos/dotfiles/z_shell
    cp ~/.tmux.conf ~/Projects/seperate_git_repos/dotfiles/z_shell
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
