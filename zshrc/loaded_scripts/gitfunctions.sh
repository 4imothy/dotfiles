gpushthistomain() {
    git add .
    git commit -m $1
    git push -u origin main
}

gpushdotfiles() {
    cp -r ~/zshrc/ ~/Projects/SeperateGitRepos/dotfiles/zshrc/loaded_scripts/.
    cp ~/.zshrc ~/Projects/SeperateGitRepos/dotfiles/zshrc
    cp ~/.tmux.conf ~/Projects/SeperateGitRepos/dotfiles/zshrc
    cp ~/.hushlogin ~/Projects/SeperateGitRepos/dotfiles/zshrc
    cp -Rf ~/.config/nvim ~/Projects/SeperateGitRepos/dotfiles
    cp -Rf ~/Documents/Vault/.obsidian/snippets/ ~/Projects/SeperateGitRepos/dotfiles/obsidian
    git -C ~/Projects/SeperateGitRepos/dotfiles add .
    git -C ~/Projects/SeperateGitRepos/dotfiles commit -m $1
    git -C ~/Projects/SeperateGitRepos/dotfiles push -u origin main
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
