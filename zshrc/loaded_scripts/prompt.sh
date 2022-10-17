NEWLINE=$'\n'
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
firstLine="%F{135}╭─%f%F{075}%/%f%k"
secondLine="%F{169}╰─%f%F{168}❱%f%F{167}❱%f%F{166}❱%f%F{173}"

PROMPT="${firstLine}${NEWLINE}${secondLine} "
export CLICOLOR=1
export LSCOLORS=fxfxcxdxbxegedabagacfx

preexec(){
    # change back to normal color
    print -Pn "%f"
}

