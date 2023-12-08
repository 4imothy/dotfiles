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
set hlsearch
set encoding=utf-8

filetype on
filetype plugin on
filetype indent on

syntax on

call plug#begin()

Plug 'sainnhe/everforest'

call plug#end()

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
