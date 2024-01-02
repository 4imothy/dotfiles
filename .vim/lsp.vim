source ~/.vim/shared_vars.vim

let g:c_syntax_for_h=1

let g:lsp_document_highlight_enabled=0
let g:lsp_semantic_enabled=1
let g:lsp_diagnostics_enabled=1
let g:lsp_completion_documentation_enabled=1
let g:lsp_completion_documentation_delay=0
let g:lsp_preview_float=1
let g:lsp_diagnostics_virtual_text_enabled=0
let g:lsp_diagnostics_float_cursor=1

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

if executable('clangd')
  au User lsp_setup call lsp#register_server({
        \   'name': 'C Language Server',
        \   'cmd': {server_info->['clangd']},
        \   'allowlist': ['C'],
        \ })
endif

if (executable('pylsp'))
    au User lsp_setup call lsp#register_server({
                \ 'name': 'pylsp',
                \ 'cmd': {server_info->['pylsp']},
                \ 'allowlist': ['python']
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
    nmap <buffer> <expr><c-u> lsp#scroll(+4)
    nmap <buffer> <expr><c-d> lsp#scroll(-4)

    let g:lsp_format_sync_timeout = 1000
    autocmd! BufWritePre *.rs,*.go call execute('LspDocumentFormatSync')
endfunction

augroup lsp_install
    au!
    autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END


let g:lsp_diagnostics_echo_cursor=0
let g:lsp_diagnostics_echo_delay=0

let g:lsp_hover_ui='float'
let g:lsp_float_max_width=-1
let g:lsp_diagnostics_float_delay=0

let g:lsp_diagnostics_virtual_text_align='right'
" let g:lsp_diagnostics_virtual_text_prefix='⨉'
let g:lsp_diagnostics_virtual_text_padding_left=1

let g:lsp_diagnostics_float_insert_mode_enabled=0
let g:lsp_diagnostics_virtual_text_insert_mode_enabled=0

let g:lsp_diagnostics_signs_enabled=1
let g:lsp_diagnostics_signs_error={'text': g:error_char}
let g:lsp_diagnostics_signs_warning={'text': g:warning_char}
let g:lsp_diagnostics_signs_info={'text': g:info_char}
let g:lsp_diagnostics_signs_hint={'text': g:hint_char}
