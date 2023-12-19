if &term =~ '256color'
    if exists("$VI_CHANGE_CURSOR_SHAPE") && exists("$VI_CHANGE_CURSOR_COLOR")
        if $VI_CHANGE_CURSOR_SHAPE == 1 || $VI_CHANGE_CURSOR_COLOR == 1
            execute 'let &t_SI .= "' . $SET_INSERT_CURSOR . '"'
            execute 'let &t_EI .= "' . $SET_NORMAL_CURSOR . '"'
        endif
    endif
endif

autocmd VimEnter * normal! i
autocmd VimResume * normal! i
