listening_ports() {
  sudo lsof -P -i -n | grep LISTEN
}

copytodotfiles(){
    cp -r ~/loaded_scripts ~/Projects/seperate_git_repos/dotfiles/z_shell/
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
