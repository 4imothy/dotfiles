# return virtual enviornment if it exists
virtualenv_info() {
    [ $VIRTUAL_ENV ] && echo "%F{12}(`basename $VIRTUAL_ENV`)%f "
    # local prefix="%F{12}$(virtualenv_info)%f"
}

final_char() {
    local prompt_string="»❱"
    echo "%(?:%F{10}$prompt_string%f:%F{9}$prompt_string%f) "
}

path() {
    echo "%F{6}$(truncate_dir)%f "
}

update_cursor() {
    echo -ne '\e[2 q'
    echo -ne '\e]12;#d3c6aa\007'
}

colored_branch() {
    local git_dir
    git_dir=$(git rev-parse --git-dir 2>/dev/null)
    # git_dir= # if delay is too long

    if [ -n "$git_dir" ]; then
        local git_status="$(git status --porcelain)"
        local current_branch="$(git rev-parse --abbrev-ref HEAD 2>/dev/null)"
        local return_status=""

        if [ -n "$git_status" ]; then
            # Red if unstaged changes
            return_status="%F{1}$current_branch%f "
        elif [ -n "$(git log origin/$current_branch $current_branch)" ]; then
            # Yellow if staged but unpushed changes
            return_status="%F{3}$current_branch%f "
        else
            # Green if up to date with this branch's origin
            return_status="%F{2}$current_branch%f "
        fi
        echo $return_status
    fi
}

# Function to truncate the current directory name
truncate_dir() {
    # Get the current directory name
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
    # Split the directory name into an array using '/' as the delimiter
    local dirs=(${(s:/:)dir})

    if [[ $dir == $HOME/* ]]; then
        local prompt="~"
        shift dirs
        shift dirs
    else
        # Initialize the prompt string
        local prompt=""
    fi
    # Iterate over the array of directory names
    for d in "${dirs[@]:0:-1}"; do
        # Truncate the directory name to the first four letters
        local truncated=${d[1,4]}
        # Append the truncated directory name to the prompt string
        prompt="$prompt/$truncated"
    done

    prompt="$prompt/${dirs[-1]}"
    # Return the prompt string
    echo $prompt
}

update_prompt() {
    local line_indicator="%F{5}✼%f "

    full="$(virtualenv_info)$(path)$(colored_branch)${line_indicator}$(final_char)%F{4}"
    PROMPT="%B${full}"
}

export CLICOLOR=1
export LSCOLORS=fxfxcxdxbxegedabagacfx

preexec(){
    # change back to normal color
    print -Pn "%f%b"
}

del_prompt_accept_line() {
    local OLD_PROMPT="$PROMPT"
    PROMPT="$(virtualenv_info)$(path)$(colored_branch)$(final_char)%F{4}"
    zle reset-prompt
    PROMPT="$OLD_PROMPT"
    zle accept-line
}

zle -N del_prompt_accept_line
bindkey "^M" del_prompt_accept_line

# Set the precmd function to update_prompt
funcs=("update_prompt" "update_cursor")
for f in "${funcs[@]}"; do
    if [[ "$precmd_functions" != *"$f"* ]]; then
        precmd_functions+=($f)
    fi
done

# autoload -Uz vcs_info
# precmd_vcs_info() { vcs_info }
# precmd_functions+=( precmd_vcs_info )
# setopt prompt_subst
# zstyle ':vcs_info:*' check-for-changes true
# zstyle ':vcs_info:git*' formats "%s  %r/%S %b (%a) %m%u%c "
# zstyle ':vcs_info:git*' formats "%b %u %c"
# RPROMPT="%f %F{229}${vcs_info_msg_0_}%f"
