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

if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
                \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    source $MYVIMRC
endif

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

for f in globpath('~/.vim', '*.vim', 0, 1, 0)
        execute 'source' f
endfor

let g:netrw_banner = 0
let g:mapleader = ","
nnoremap <leader>n :bnext<cr>
nnoremap <leader>p :bprevious<cr>
nnoremap <leader>d :bdelete<cr>
nnoremap <leader>t :term++rows=10<cr>
nnoremap <leader>s :Explore<CR>
nnoremap <leader>e :LspDocumentDiagnostics<CR>
imap <expr> <Tab> vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : (pumvisible() ? asyncomplete#close_popup() : "\<Tab>")
smap <expr> <Tab> vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : (pumvisible() ? asyncomplete#close_popup() : "\<Tab>")

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
