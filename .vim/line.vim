source ~/.vim/shared_vars.vim

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = ' '
let g:airline#extensions#tabline#right_sep = ''
let g:airline#extensions#tabline#right_alt_sep = ''
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline#extensions#lsp#enabled=1
let airline#extensions#lsp#error_symbol=g:error_char
let airline#extensions#lsp#warning_symbol=g:warning_char
if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
let g:airline_symbols.branch = ''
let g:airline_symbols.dirty=''
let g:airline#extensions#tabline#ignore_bufadd_pat = 'defx|gundo|nerd_tree|startify|tagbar|undotree|vimfiler'
let g:airline_section_z='%3p%%'
let g:airline#extensions#wordcount#filetypes=[]

let g:airline_mode_map = {
  \ '__' : '-',
  \ 'n' : 'N',
  \ 'niI' : 'N',
  \ 'i' : 'I',
  \ 'R' : 'R',
  \ 'c' : 'C',
  \ 'v' : 'V',
  \ 'V' : 'V',
  \ '' : 'V',
  \ 's' : 'S',
  \ 'S' : 'S',
  \ '' : 'S',
  \ }

" custom status line
" function! s:statusline_expr()
  " let mod = "%{&modified ? '[+] ' : !&modifiable ? '[x] ' : ''}"
  " let ro  = "%{&readonly ? '[RO] ' : ''}"
  " let ft  = "%{len(&filetype) ? '['.&filetype.'] ' : ''}"

  " let sep = ' %= '

  " let fug = "%{exists('g:loaded_fugitive') ? fugitive#statusline() : ''}"
  " let enc = "%{&fileencoding !=# '' ? '['.&fileencoding.'] ' : ''}"
  " let le  = "%{&fileformat !=# '' ? '['.&fileformat.'] ' : ''}"
  " let le = "%{&fileformat !=# '' ? '['.&fileformat.']' : ''}%*"


  " return ' %f %<'.mod.ro.ft.fug.sep.enc.le
" endfunction

" let &statusline = s:statusline_expr()
