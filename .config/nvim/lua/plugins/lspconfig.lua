local signs = { Error = "󰅚", Warn = "󰀪", Hint = "󰌶", Info = "" }
return {
    'neovim/nvim-lspconfig',
    dependencies = {
        'saghen/blink.cmp'
    },
    config = function()
        local capabilities = require('blink.cmp').get_lsp_capabilities()
        local lspconfig = require('lspconfig')
        lspconfig.pyright.setup({
            capabilities = capabilities
        })
        lspconfig.racket_langserver.setup {
            capabilities = capabilities
        }
        lspconfig.tinymist.setup {
            capabilities = capabilities
        }
        lspconfig.clangd.setup({
            capabilities = capabilities
        })
        lspconfig.rust_analyzer.setup({
            capabilities = capabilities
        })
        lspconfig.texlab.setup({
            capabilities = capabilities
        })
        lspconfig.hls.setup({
            capabilities = capabilities
        })
        lspconfig.ltex.setup({
            capabilities = capabilities,
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
        })
        vim.api.nvim_create_autocmd('LspAttach', {
            group = vim.api.nvim_create_augroup('UserLspConfig', {}),
            callback = function(ev)
                local opts = { buffer = ev.buf }
                vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
                vim.keymap.set('n', 'gn', vim.lsp.buf.rename, opts)
                vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
                vim.keymap.set({ 'n', 'v' }, 'ga', vim.lsp.buf.code_action, opts)
            end,
        })

        vim.cmd [[autocmd BufWritePre * lua vim.lsp.buf.format()]]
    end,
}
