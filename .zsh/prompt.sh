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

update_cursor() {
    echo -n "\e]12;$DEFAULT_CURSOR_COLOR\007"
}

colored_branch() {
    if [ $SHOW_BRANCH_IN_PROMPT -eq 1 ]; then
        local git_dir=$(git rev-parse --git-dir 2>/dev/null)
        if [ -n "$git_dir" ]; then
            local git_status="$(git status --porcelain)"
            local current_branch="$(git rev-parse --abbrev-ref HEAD 2>/dev/null)"
            local return_status=""

            if [ -n "$git_status" ]; then
                return_status="%F{1}$current_branch%f "
            elif [ -n "$(git log origin/$current_branch..$current_branch)" ]; then
                return_status="%F{3}$current_branch%f "
            else
                return_status="%F{2}$current_branch%f "
            fi
            echo $return_status
        fi
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

update_prompt() {
    full="$(virtualenv_info)$(path)$(colored_branch)$(line_indicator)$(final_char)%F{4}"
    PROMPT="%B${full}"
}

del_prompt_accept_line() {
    local OLD_PROMPT="$PROMPT"

    OLD_PROMPT="${OLD_PROMPT//\%B}"
    OLD_PROMPT="${OLD_PROMPT//$(line_indicator)}"

    PROMPT="$OLD_PROMPT"
    zle reset-prompt
    zle accept-line
}

zle -N del_prompt_accept_line
bindkey "^M" del_prompt_accept_line

export CLICOLOR=1
export LSCOLORS=fxfxcxdxbxegedabagacfx

preexec(){
    # change back to normal color
    print -Pn "%f%b"
}

# Set the precmd function to update_prompt
funcs=("update_prompt" "update_cursor")
for f in "${funcs[@]}"; do
    if [[ "$precmd_functions" != *"$f"* ]]; then
        precmd_functions+=($f)
    fi
done
