listening_ports() {
  sudo lsof -P -i -n | grep LISTEN
}

copytodotfiles(){
    cp -r ~/loaded_scripts ~/Projects/seperate_git_repos/dotfiles/zshrc/
    cp ~/.zshrc ~/Projects/seperate_git_repos/dotfiles/zshrc
    cp ~/.tmux.conf ~/Projects/seperate_git_repos/dotfiles/zshrc
    cp ~/.hushlogin ~/Projects/seperate_git_repos/dotfiles/zshrc
    cp -Rf ~/.config/nvim ~/Projects/seperate_git_repos/dotfiles
    cp -Rf ~/Documents/Vault/.obsidian/snippets/ ~/Projects/seperate_git_repos/dotfiles/obsidian
}
