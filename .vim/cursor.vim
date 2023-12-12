set t_u7=
if exists("$VI_CHANGE_CURSOR_SHAPE") && exists("$VI_CHANGE_CURSOR_COLOR")
    if $VI_CHANGE_CURSOR_SHAPE == 1
        silent! let &t_SI="\e[6 q"
        silent! let &t_EI="\e[2 q"
        autocmd VimEnter * silent! !echo -ne "\e[2 q"
    endif

    if $VI_CHANGE_CURSOR_COLOR == 1
        silent! let &t_SI.="\e]12;" . $DEFAULT_CURSOR_COLOR . "\007"
        silent! let &t_EI.="\e]12;" . $NORMAL_CURSOR_COLOR . "\007"
        autocmd VimEnter * silent! !echo -ne "\e]12;$NORMAL_CURSOR_COLOR\007"
    endif
endif

