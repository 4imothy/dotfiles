export FZF_DEFAULT_COMMAND="rg --files --hidden --follow --glob '!.git'"
export FZF_DEFAULT_OPTS='-m --height 50% --border --layout=reverse'

_add_fzf_colors() {
    local colors="
    --color=bg+:-1,spinner:10,hl:11
    --color=fg+:7,header:11,info:10,pointer:6
    --color=marker:6,fg+:14,prompt:10,hl+:11
    --color=border:15"

    export FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS}${colors}"
}

_add_fzf_colors
