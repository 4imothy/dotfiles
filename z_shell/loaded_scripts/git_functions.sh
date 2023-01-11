gpushthistomain() {
    git add .
    git commit -m $1
    git push -u origin main
}

gpushdotfiles() {
    cp -r ~/loaded_scripts ~/Projects/seperate_git_repos/dotfiles/z_shell/
    cp -r ~/.qutebrowser/ ~/Projects/seperate_git_repos/dotfiles/browsers/qutebrowser
    cp ~/Library/Application\ Support/Firefox/Profiles/ry1zhwm6.default-release/chrome/userChrome.css ~/Projects/seperate_git_repos/dotfiles/browsers/firefox/
    cp ~/.zshrc ~/Projects/seperate_git_repos/dotfiles/z_shell
    cp ~/.tmux.conf ~/Projects/seperate_git_repos/dotfiles/z_shell
    cp ~/.hushlogin ~/Projects/seperate_git_repos/dotfiles/z_shell
    cp -Rf ~/.config/nvim ~/Projects/seperate_git_repos/dotfiles
    cp ~/.config/alacritty/alacritty.yml ~/Projects/seperate_git_repos/dotfiles/terminals/
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
