gpushthistomain() {
    git add .
    git commit -m $1
    git push -u origin main
}

copytodotfiles(){
    cp -r ~/loaded_scripts ~/Projects/seperate_git_repos/dotfiles/zshrc/
    cp ~/.zshrc ~/Projects/seperate_git_repos/dotfiles/zshrc
    cp ~/.tmux.conf ~/Projects/seperate_git_repos/dotfiles/zshrc
    cp ~/.hushlogin ~/Projects/seperate_git_repos/dotfiles/zshrc
    cp -Rf ~/.config/nvim ~/Projects/seperate_git_repos/dotfiles
    cp -Rf ~/Documents/Vault/.obsidian/snippets/ ~/Projects/seperate_git_repos/dotfiles/obsidian
}

gpushdotfiles() {
    cp -r ~/loaded_scripts ~/Projects/seperate_git_repos/dotfiles/zshrc/
    cp ~/.zshrc ~/Projects/seperate_git_repos/dotfiles/zshrc
    cp ~/.tmux.conf ~/Projects/seperate_git_repos/dotfiles/zshrc
    cp ~/.hushlogin ~/Projects/seperate_git_repos/dotfiles/zshrc
    cp -Rf ~/.config/nvim ~/Projects/seperate_git_repos/dotfiles
    cp -Rf ~/Documents/Vault/.obsidian/snippets/ ~/Projects/seperate_git_repos/dotfiles/obsidian

    git -C ~/Projects/seperate_git_repos/dotfiles add .
    git -C ~/Projects/seperate_git_repos/dotfiles commit -m $1
    git -C ~/Projects/seperate_git_repos/dotfiles push -u origin main
}

gpushknowledgebase() {
    git -C ~/Documents/Vault/🏫\ SchoolNotes add .
    git -C ~/Documents/Vault/🏫\ SchoolNotes commit -m $1
    git -C ~/Documents/Vault/🏫\ SchoolNotes  push -u origin main
}

gpushmushroomdriverscripts() {
    cp -r ~/Projects/Mushroom\ Driver/Mushroom\ Driver/Assets/Scripts ~/Projects/SeperateGitRepos/Mushroom\ Driver
    git -C ~/Projects/SeperateGitRepos/Mushroom\ Driver add .
    git -C ~/Projects/SeperateGitRepos/Mushroom\ Driver commit -m $1
    git -C ~/Projects/SeperateGitRepos/Mushroom\ Driver push -u origin main
}
