set number relativenumber
set ai
set si
:set shiftwidth=4
set nocompatible
set noshowmode
set nohlsearch
set scrolloff=7
set signcolumn=yes
set colorcolumn=80

filetype off
filetype indent on

call plug#begin('~/.config/nvim/plugged')

Plug 'tpope/vim-fugitive'

Plug 'preservim/nerdtree'
Plug 'ryanoasis/vim-devicons'
Plug 'airblade/vim-gitgutter'

Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'HerringtonDarkholme/yats.vim' " typescript syntax
Plug 'pangloss/vim-javascript' " javascript syntax
Plug 'MaxMEllon/vim-jsx-pretty' " jsx
Plug 'dense-analysis/ale'
Plug 'vim-airline/vim-airline'

Plug 'ctrlpvim/ctrlp.vim'
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app && yarn install' }
Plug 'morhetz/gruvbox'

call plug#end()

colorscheme gruvbox
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ }

nnoremap <C-n> :NERDTreeFocus<CR>

" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup
set updatetime=100

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
set signcolumn=yes
" Make <CR> to accept selected completion item or notify coc.nvim to format
inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

if has("autocmd")
    " au VimLeave * silent execute '!echo -ne #"\e[5 q"' | redraw!
    " au VimLeave * silent !echo -ne #"\e[5 q"
    au VimLeave * silent execute 'echo "\e[5 q"' | redraw!
endif

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)

" Use K to show documentation in preview window.
" inoremap <silent> K :call ShowDocumentation()<CR>

function! ShowDocumentation()
  if CocAction('hasProvider', 'hover')
    call CocActionAsync('doHover')
  else
    call feedkeys('K', 'in')
  endif
endfunction
