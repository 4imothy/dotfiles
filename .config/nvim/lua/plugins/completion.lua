return {
    'hrsh7th/nvim-cmp',
    dependencies = {
        'hrsh7th/cmp-nvim-lsp',
        'hrsh7th/cmp-path',
        'hrsh7th/cmp-cmdline',
        'hrsh7th/cmp-buffer',
        'hrsh7th/cmp-vsnip',
        'hrsh7th/vim-vsnip'
    },
    config = function()
        vim.g.vsnip_snippet_dir = vim.fn.stdpath('config') .. '/snippets'
        local cmp = require('cmp')
        cmp.setup({
            snippet = {
                expand = function(args)
                    vim.fn['vsnip#anonymous'](args.body)
                end
            },
            window = {
                documentation = cmp.config.window.bordered(),
            },
            mapping = cmp.mapping.preset.insert({
                ['<Tab>'] = cmp.mapping.confirm({ select = true }),
                ['<C-r>'] = cmp.mapping.scroll_docs(-4),
                ['<C-f>'] = cmp.mapping.scroll_docs(4),
            }),
            sources = cmp.config.sources({
                { name = 'nvim_lsp' },
                { name = 'vsnip' },
                { name = 'path' },
                { name = 'neorg' },
                { name = 'orgmode' },
            }),
            performance = {
                max_view_entries = 10
            },
            formatting = {
                fields = { "abbr", "menu", "kind" },
                format = function(entry, item)
                    local menu_icon = {
                        nvim_lsp = "L",
                        vsnip = "S",
                        buffer = "B",
                        path = "P",
                    }
                    item.menu = menu_icon[entry.source.name]
                    fixed_width = 40
                    fixed_width = fixed_width or false
                    local content = item.abbr
                    if fixed_width then
                        vim.o.pumwidth = fixed_width
                    end
                    local win_width = vim.api.nvim_win_get_width(0)
                    local max_content_width = fixed_width and fixed_width - 10 or math.floor(win_width * 0.2)
                    if #content > max_content_width then
                        item.abbr = vim.fn.strcharpart(content, 0, max_content_width - 3) .. "..."
                    else
                        item.abbr = content .. (" "):rep(max_content_width - #content)
                    end
                    return item
                end,
            },
            enabled = function()
                local context = require 'cmp.config.context'
                if vim.api.nvim_get_mode().mode == 'c' then
                    return true
                else
                    return not context.in_treesitter_capture("comment")
                    and not context.in_syntax_group("Comment")
                end
            end
        })

        cmp.setup.cmdline({ '/', '?' }, {
            mapping = cmp.mapping.preset.cmdline(),
            sources = {
                { name = 'buffer' }
            }
        })

        cmp.setup.cmdline(':', {
            mapping = cmp.mapping.preset.cmdline(),
            sources = cmp.config.sources({
                { name = 'path' }
            }, {
                { name = 'cmdline', keyword_length = 2 }
            })
        })
        -- TODO this shouldn't be necessary I think check after some updates, added 3/4/2024
        vim.api.nvim_create_autocmd("FileType", {
            pattern = { "TelescopePrompt" },
            callback = function()
                cmp.setup.buffer({ enabled = false })
            end,
            once = false,
        })
    end
}
