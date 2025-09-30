local signs = { Error = "󰅚", Warn = "󰀪", Hint = "󰌶", Info = "" }
return {
    'neovim/nvim-lspconfig',
    dependencies = {
        'saghen/blink.cmp'
    },
    config = function()
        local capabilities = require('blink.cmp').get_lsp_capabilities()
        vim.lsp.config('*', {
            capabilities = capabilities
        })
        vim.lsp.enable('pyright')
        vim.lsp.enable('racket_langserver')
        vim.lsp.enable('tinymist')
        vim.lsp.enable('clangd')
        vim.lsp.enable('rust_analyzer')
        vim.lsp.enable('texlab')
        vim.lsp.enable('hls')
        vim.lsp.enable('ltex')
        vim.lsp.config['ltex'] = {
            filetypes= { 'bib', 'gitcommit', 'markdown', 'org', 'plaintex', 'tex', 'html', 'txt'},
            settings = {
                ltex = {
                    language = 'auto',
                    diagnosticSeverity = 'information',
                    additionalRules = {
                        languageModel = '~/Projects/dotfiles/ngrams/',
                    },
                    disabledRules = {
                        ['en-US'] = {
                            'MORFOLOGIK_RULE_EN_US',
                        },
                    },
                },
            }
        }
        -- lspconfig.harper_ls.setup({
        --     capabilities = capabilities
        -- })

        vim.api.nvim_create_autocmd('LspAttach', {
            group = vim.api.nvim_create_augroup('UserLspConfig', {}),
            callback = function(ev)
                local opts = { buffer = ev.buf }
                vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
                vim.keymap.set('n', 'gn', vim.lsp.buf.rename, opts)
                vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
                vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
                vim.keymap.set({ 'n', 'v' }, 'ga', vim.lsp.buf.code_action, opts)
            end,
        })

        vim.cmd [[
            autocmd BufWritePre * lua if vim.api.nvim_win_get_width(0) >= 80 then vim.lsp.buf.format() end
        ]]
    end,
}
