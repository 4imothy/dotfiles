if &term =~ '256color'
    if exists("$VI_CHANGE_CURSOR_SHAPE") && exists("$VI_CHANGE_CURSOR_COLOR")
        if $VI_CHANGE_CURSOR_SHAPE == 1
            let &t_SI="\e[6 q"
            let &t_EI="\e[2 q"
        endif

        if $VI_CHANGE_CURSOR_COLOR == 1
            let &t_SI.="\e]12;" . $DEFAULT_CURSOR_COLOR . "\x7"
            let &t_EI.="\e]12;" . $NORMAL_CURSOR_COLOR . "\x7"
        endif
    endif
endif

autocmd VimEnter * call EnterNormal()

function! EnterNormal()
    normal! i
endfunction
