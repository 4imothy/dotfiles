# return virtual enviornment if it exists
function virtualenv_info {
    [ $VIRTUAL_ENV ] && echo '('`basename $VIRTUAL_ENV`') '
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
    # Set the PROMPT variable to the truncated version of the current directory
    local prefix="%F{12}$(virtualenv_info)%f"
    # local path_string="%F{075}~%f"
    local path_string="%F{14}$(truncate_dir)%f"
    local prompt_string="»❱"
    # »❱ 
    local line_indicator="%F{5}✼%f"
    #㊀⚛
    # Make prompt_string red if the previous command failed.
    local return_status="%(?:%F{10}$prompt_string%f:%F{9}$prompt_string%f)"

    full="${prefix} ${path_string} ${line_indicator} ${return_status}%F{4}"
    PROMPT="%B${full} "
}

export CLICOLOR=1
export LSCOLORS=fxfxcxdxbxegedabagacfx

preexec(){
    # change back to normal color
    print -Pn "%f%b"
}

del-prompt-accept-line() {
    local prefix="%F{12}$(virtualenv_info)%f"
    local prompt_string="»❱"
    local return_status="%(?:%F{10}$prompt_string%f:%F{9}$prompt_string%f)"
    local path_string="%F{6}$(truncate_dir)%f"
    local OLD_PROMPT="$PROMPT"
    PROMPT="${prefix} ${path_string} ${return_status}%F{4} "
    zle reset-prompt
    PROMPT="$OLD_PROMPT"
    zle accept-line
}
zle -N del-prompt-accept-line
bindkey "^M" del-prompt-accept-line

# Set the precmd function to update_prompt
if [[ ! "$precmd_functions" == *update_prompt* ]]; then
    precmd_functions+=(update_prompt)
fi

# autoload -Uz vcs_info
# precmd_vcs_info() { vcs_info }
# precmd_functions+=( precmd_vcs_info )
# setopt prompt_subst
# zstyle ':vcs_info:*' check-for-changes true
# zstyle ':vcs_info:git*' formats "%s  %r/%S %b (%a) %m%u%c "
# zstyle ':vcs_info:git*' formats "%b %u %c"
# RPROMPT="%f %F{229}${vcs_info_msg_0_}%f"