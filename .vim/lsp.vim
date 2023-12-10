if executable('texlab')
    au User lsp_setup call lsp#register_server({
                \ 'name': 'texlab',
                \ 'cmd': {server_info->['texlab']},
                \ 'allowlist': ['tex', 'bib', 'sty'],
                \ })
endif

if executable('rust-analyzer')
  au User lsp_setup call lsp#register_server({
        \   'name': 'Rust Language Server',
        \   'cmd': {server_info->['rust-analyzer']},
        \   'allowlist': ['rust'],
        \ })
endif

function! s:on_lsp_buffer_enabled()
    setlocal omnifunc=lsp#complete
    if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
    nmap <buffer> gd <plug>(lsp-definition)
    nmap <buffer> gs <plug>(lsp-document-symbol-search)
    nmap <buffer> gS <plug>(lsp-workspace-symbol-search)
    nmap <buffer> gr <plug>(lsp-references)
    nmap <buffer> gi <plug>(lsp-implementation)
    nmap <buffer> gt <plug>(lsp-type-definition)
    nmap <buffer> <leader>rn <plug>(lsp-rename)
    nmap <buffer> [g <plug>(lsp-previous-diagnostic)
    nmap <buffer> ]g <plug>(lsp-next-diagnostic)


    let g:lsp_format_sync_timeout = 1000
    autocmd! BufWritePre *.rs,*.go call execute('LspDocumentFormatSync')
endfunction

augroup lsp_install
    au!
    autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END

let g:lsp_diagnostics_virtual_text_enabled = 1
let g:lsp_diagnostics_virtual_text_insert_mode_enabled = 1
let g:lsp_diagnostics_virtual_text_align = "right"
let g:lsp_diagnostics_virtual_text_prefix = 'ℹ '
let g:lsp_diagnostics_virtual_text_padding_left = 3
" let g:lsp_diagnostics_float_cursor = 1
" let g:lsp_diagnostics_float_delay = 500
" let g:lsp_diagnostics_echo_cursor = 1
let g:lsp_diagnostics_signs_enabled = 0
let g:lsp_diagnostics_signs_error = {'text': '⨉'}
let g:lsp_diagnostics_signs_warning = {'text': '‼'}
let g:lsp_diagnostics_signs_info = {'text': 'ℹ'}
let g:lsp_diagnostics_signs_hint = {'text': '?'}
