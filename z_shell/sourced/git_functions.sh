gacp() {
    git add .
    git commit -m $1
    git push -u origin main
}

gac() {
    if [ -n "$1" -a -n "$2" ]; then
        git add $1
        git commit -m "$2"
    fi
}
 
gpushdotfiles() {
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
