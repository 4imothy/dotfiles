set t_ZH=[3m
set t_ZR=[23m
set number relativenumber
set nocompatible
set nobackup
set nowritebackup
set shiftwidth=4
set tabstop=4
set expandtab
set wrap
set incsearch
set ignorecase
set smartcase
set signcolumn=no
set hlsearch
set encoding=utf-8
set timeoutlen=1000
set ttimeoutlen=0
set clipboard+=unnamed
set splitbelow

filetype on
filetype plugin on
filetype indent on

syntax on
call plug#begin()

Plug 'sainnhe/everforest'
Plug 'vim-airline/vim-airline'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'hrsh7th/vim-vsnip'
Plug 'hrsh7th/vim-vsnip-integ'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/vim-lsp'

call plug#end()

let g:mapleader = ","
map <leader>n :bnext<cr>
map <leader>p :bprevious<cr>
map <leader>d :bdelete<cr>
map <leader>t :term++rows=10<cr>
nnoremap <silent> <leader>e :LspDocumentDiagnostics<CR>
imap <expr> <Tab> vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : (pumvisible() ? asyncomplete#close_popup() : "\<Tab>")
smap <expr> <Tab> vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : (pumvisible() ? asyncomplete#close_popup() : "\<Tab>")

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

let g:vsnip_snippet_dir = expand('~/.vim/snippets')

let g:airline#extensions#tabline#enabled = 1
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = ''
if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
let g:airline_symbols.branch = ''
let g:airline_symbols.dirty=''

if has('termguicolors')
    set termguicolors
endif
set background=dark
let g:everforest_background = 'soft'
let g:everforest_better_performance = 1
colorscheme everforest

function TrimWhiteSpace()
    let l:save = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:save)
endfunction

autocmd BufWritePre * :call TrimWhiteSpace()
autocmd BufNewFile,BufRead *.tex :set filetype=tex
autocmd FileType tex,txt,md setlocal spell spelllang=en_us
