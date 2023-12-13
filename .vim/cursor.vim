if &term =~ '256color'
    if exists("$VI_CHANGE_CURSOR_SHAPE") && exists("$VI_CHANGE_CURSOR_COLOR")
        if $VI_CHANGE_CURSOR_SHAPE == 1
            execute 'let &t_SI .= "' . $SET_BEAM_CURSOR . '"'
            execute 'let &t_EI .= "' . $SET_BLOCK_CURSOR . '"'
        endif

        if $VI_CHANGE_CURSOR_COLOR == 1
            execute 'let &t_SI .= "' . $SET_DEFAULT_CURSOR_COLOR . '"'
            execute 'let &t_EI .= "' . $SET_NORMAL_CURSOR_COLOR . '"'
        endif
    endif
endif

autocmd VimEnter * call EnterNormal()

function! EnterNormal()
    normal! i
endfunction
