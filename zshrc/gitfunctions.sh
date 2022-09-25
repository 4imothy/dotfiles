pushDotfiles() {
    cp ~/.zshrc ~/.config/dotfiles
    cp ~/.tmux.conf ~/.config/dotfiles
    cp -R ~/zshrc ~/.config/dotfiles
    cp -Rf ~/.config/nvim ~/.config/dotfiles
    cp -Rf ~/Documents/Vault/.obsidian/snippets/ ~/.config/dotfiles/obsidian
    git -C ~/.config/dotfiles add .
    git -C ~/.config/dotfiles commit -m $1
    git -C ~/.config/dotfiles push -u origin main
}

pushSchoolNotes() {
    git -C ~/Documents/Vault/🏫\ SchoolNotes add .
    git -C ~/Documents/Vault/🏫\ SchoolNotes commit -m $1
    git -C ~/Documents/Vault/🏫\ SchoolNotes  push -u origin main
}

pushMushroomDriverScripts() {
    cp -r ~/Projects/Mushroom\ Driver/Mushroom\ Driver/Assets/Scripts ~/Projects/SeperateGitRepos/Mushroom\ Driver
    git -C ~/Projects/SeperateGitRepos/Mushroom\ Driver add .
    git -C ~/Projects/SeperateGitRepos/Mushroom\ Driver commit -m $1
    git -C ~/Projects/SeperateGitRepos/Mushroom\ Driver push -u origin main
}
