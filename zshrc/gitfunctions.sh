pushDotfiles() {                                        
    cp /Users/tcron/.zshrc /Users/tcron/.config/dotfiles          
    cp -R /Users/tcron/zshrc /Users/tcron/.config/dotfiles          
    cp -Rf /Users/tcron/.config/nvim /Users/tcron/.config/dotfiles          
    cp -Rf ~/Documents/Vault/.obsidian/snippets/ ~/.config/dotfiles/obsidian
    git -C /Users/tcron/.config/dotfiles add .                              
    git -C /Users/tcron/.config/dotfiles commit -m $1                       
    git -C /Users/tcron/.config/dotfiles push -u origin main
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
