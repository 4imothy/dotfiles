# NEWLINE=$'\n'
# teal colors
# firstLine="%F{049}╭─%/%f"
# secondLine="%F{043}╰─❱%f%F{037}❱%f%F{031}❱%f %F{111}"

# lavender with red yellow green
# firstLine="%F{105}╭─%/%f%k"
# secondLine="%F{104}╰─%f%F{9}❱%f%F{11}❱%f%F{10}❱%f%F{123} "

# grayscale
# firstLine="%F{242}╭─%/%f%k"
# secondLine="%F{243}╰─%f%F{244}❱%f%F{245}❱%f%F{246}❱%f%F{248}

#
# firstLine="%F{214}╭─%f%F{076}%/%f%k"
# secondLine="%F{214}╰─%f%F{166}❱%f%F{167}❱%f%F{168}❱%f%F{191}"

# sunset
# firstLine="%F{135}╭─%f%F{075}%/%f%k"
# secondLine="%F{169}╰─%f%F{168}❱%f%F{167}❱%f%F{166}❱%f%F{173}"

# PROMPT="${firstLine}${NEWLINE}${secondLine} "


# local prefix="%F{073}%f%F{074}❱%f"
local prefix=""
local path_string="%F{075}%~%f"
local prompt_string="»❱"

# Make prompt_string red if the previous command failed.
local return_status="%(?:%F{114}$prompt_string%f:%F{196}$prompt_string%f)"

full="${prefix} ${path_string} ${return_status}%F{177}"
PROMPT="%B${full} "

export CLICOLOR=1
export LSCOLORS=fxfxcxdxbxegedabagacfx

preexec(){
    # change back to normal color
    print -Pn "%f%b"
}
del-prompt-accept-line() {
    OLD_PROMPT="$PROMPT"
    PROMPT="${full} "
    zle reset-prompt
    PROMPT="$OLD_PROMPT"
    zle accept-line
}
zle -N del-prompt-accept-line
bindkey "^M" del-prompt-accept-line
# autoload -Uz vcs_info
# precmd_vcs_info() { vcs_info }
# precmd_functions+=( precmd_vcs_info )
# setopt prompt_subst
# zstyle ':vcs_info:*' check-for-changes true
# zstyle ':vcs_info:git*' formats "%s  %r/%S %b (%a) %m%u%c "
# zstyle ':vcs_info:git*' formats "%b %u %c"
# RPROMPT="%f %F{229}${vcs_info_msg_0_}%f"
