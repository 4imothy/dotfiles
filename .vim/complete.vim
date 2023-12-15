autocmd User asyncomplete_setup call asyncomplete#register_source(asyncomplete#sources#file#get_source_options({
    \ 'name': 'file',
    \ 'allowlist': ['*'],
    \ 'priority': 10,
    \ 'completor': function('asyncomplete#sources#file#completor')
    \ }))

let g:asyncomplete_auto_popup=1
let g:asyncomplete_completion_delay=0
let g:asyncomplete_min_chars=0

function! s:my_asyncomplete_preprocessor(options, matches) abort
    let l:visited = {}
    let l:items = []
    for [l:source_name, l:matches] in items(a:matches)
        for l:item in l:matches['items']
            if stridx(l:item['word'], a:options['base']) == 0
                if !has_key(l:visited, l:item['word'])
                    call add(l:items, l:item)
                    let l:visited[l:item['word']] = 1
                endif
            endif
        endfor
    endfor

    call asyncomplete#preprocess_complete(a:options, l:items)
endfunction

let g:asyncomplete_preprocessor = [function('s:my_asyncomplete_preprocessor')]
