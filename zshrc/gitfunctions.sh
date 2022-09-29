pushTennisGame() {
    git -C ~/Projects/TennisGame add .
    git -C ~/Projects/TennisGame commit -m $1
    git -C ~/Projects/TennisGame push -u origin main
}

pushDotfiles() {
    cp ~/.zshrc ~/Projects/SeperateGitRepos/dotfiles
    cp ~/.tmux.conf ~/Projects/SeperateGitRepos/dotfiles
    cp -R ~/zshrc ~/Projects/SeperateGitRepos/dotfiles
    cp -Rf ~/.config/nvim ~/Projects/SeperateGitRepos/dotfiles
    cp -Rf ~/Documents/Vault/.obsidian/snippets/ ~/Projects/SeperateGitRepos/dotfiles/obsidian
    git -C ~/Projects/SeperateGitRepos/dotfiles add .
    git -C ~/Projects/SeperateGitRepos/dotfiles commit -m $1
    git -C ~/Projects/SeperateGitRepos/dotfiles push -u origin main
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
