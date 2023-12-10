# Base16 Everforest
# Scheme author: Sainnhe Park (https://github.com/sainnhe)
# Template author: Tinted Theming (https://github.com/tinted-theming)

export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'

_add_fzf_colors() {
    local colors="
    --color=bg+:-1,spinner:10,hl:11
    --color=fg+:7,header:11,info:10,pointer:6
    --color=marker:6,fg+:14,prompt:10,hl+:11
    --color=border:15"

    export FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS}${colors}"
}

_add_fzf_colors
