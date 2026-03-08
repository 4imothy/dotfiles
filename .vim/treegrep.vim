Plug '4imothy/treegrep', {'do': {-> TgrepBuild()}}

let g:tgrep_selection_file = '/tmp/tgrep-select'
let g:tgrep_repeat_file = '/tmp/tgrep-repeat'

nnoremap <leader>tt :call TgrepWith('--menu')<cr>
nnoremap <leader>tr :call TgrepWith('--repeat --select')<cr>
nnoremap <leader>tm :call TgrepWith('--menu --repeat')<cr>
nnoremap <leader>tf :call TgrepWith('--files --select')<cr>
