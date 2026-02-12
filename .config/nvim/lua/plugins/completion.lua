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
                            width = { min = 20, max = 20 },
                        }
                    }
                }
            }
        },
        sources = {
            default = { 'snippets', 'lsp', 'path', 'buffer' },
        },
    },
}
