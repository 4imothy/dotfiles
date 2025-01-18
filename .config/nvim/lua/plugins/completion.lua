return {
    'saghen/blink.cmp',
    version = '*',
    opts = {
        keymap = { preset = 'default' },
        completion = {
            menu = {
                max_height = 5,
                draw = {
                    components = {
                        label = {
                            ellipsis = true,
                            width = {
                                min = 20,
                                max = 20,
                            },
                        }
                    }
                }
            }
        },
        sources = {
            default = { 'snippets', 'lsp', 'path', 'buffer' },
            providers = {
                path = {
                    name = 'path',
                    module = 'blink.cmp.sources.path',
                    score_offset = 4
                },
                snippets = {
                    name = 'snippets',
                    module = 'blink.cmp.sources.snippets',
                    score_offset = 3
                },
                lsp = {
                    name = 'lsp',
                    module = 'blink.cmp.sources.lsp',
                    score_offset = 2
                },
                buffer = {
                    name = 'buffer',
                    module = 'blink.cmp.sources.buffer',
                    score_offset = 1,
                    min_keyword_length = 5
                }
            }
        },
    },
}
