virtualenv_info() {
    [ $VIRTUAL_ENV ] && echo "%F{12}(`basename $VIRTUAL_ENV`)%f "
}

final_char() {
    local prompt_string="»❱"
    echo "%(?:%F{10}$prompt_string%f:%F{9}$prompt_string%f) "
}

path() {
    echo "%F{6}$(truncate_dir)%f "
}

line_indicator() {
    echo "%F{5}✼%f "
}

set_cursor_color() {
    echo -ne $SET_DEFAULT_CURSOR_COLOR
}

colored_branch() {
    local git_dir=$(git rev-parse --git-dir 2>/dev/null)
    if [ -n "$git_dir" ]; then
        local git_status="$(git status --porcelain)"
        local current_branch="$(git rev-parse --abbrev-ref HEAD 2>/dev/null)"
        local return_status=""

        if [ -n "$git_status" ]; then
            return_status="%F{1}$current_branch%f"
        elif [ -n "$(git log origin/$current_branch..$current_branch)" ]; then
            return_status="%F{3}$current_branch%f"
        else
            return_status="%F{2}$current_branch%f"
        fi
        echo $return_status
    fi
}

truncate_dir() {
    local dir=$(pwd)

    if [[ "$dir" == "/" ]]; then
        local prompt="/"
        echo $prompt
        return
    fi

    if [[ "$dir" == "$HOME" ]]; then
        local prompt="~"
        echo $prompt
        return
    fi
    local dirs=(${(s:/:)dir})

    if [[ $dir == $HOME/* ]]; then
        local prompt="~"
        shift dirs
        shift dirs
    else
        local prompt=""
    fi

    for d in "${dirs[@]:0:-1}"; do
        local truncated=${d[1,PAR_DIR_PRINT_LIMIT]}
        prompt="$prompt/$truncated"
    done

    prompt="$prompt/${dirs[-1]}"
    echo $prompt
}

fg_color() {
    if [ $SUGGESTIONS -eq 1 ]; then
        local fg_color="%F{4}"
    else
        local fg_color=""
    fi
}

update_prompt() {
    local branch=""
    if [ $SHOW_BRANCH_IN_PROMPT -eq 1 ]; then
        branch="$(colored_branch) "
    fi

    full="$(virtualenv_info)$(path)$branch$(line_indicator)$(final_char)$(fg_color)"
    PROMPT="%B${full}"
    set_rp
    tput cnorm
}

reset_fg(){
    print -Pn "%f%b"
}

del_prompt_accept_line() {
    tput civis
    local OLD_PROMPT="$PROMPT"

    OLD_PROMPT="${OLD_PROMPT//\%B}"
    OLD_PROMPT="${OLD_PROMPT//$(line_indicator)}"

    PROMPT="$OLD_PROMPT"
    zle reset-prompt
    zle accept-line
}

set_rp() {
    if [ $SHOW_BRANCH_IN_RPROMPT -eq 1 ]; then
        branch=$(colored_branch)
        RPROMPT="%F{7}❮%f${branch:+ }${branch}%F{7}❯%f"
    fi
}

zle -N del_prompt_accept_line
bindkey "^M" del_prompt_accept_line

add_to_array precmd_functions "update_prompt"
add_to_array precmd_functions "set_cursor_color"
if [ $SUGGESTIONS -eq 0 ]; then
    add_to_array preexec_functions "reset_fg"
fi
add_to_array preexec_functions "set_cursor_color"
