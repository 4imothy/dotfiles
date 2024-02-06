return {
    'neovim/nvim-lspconfig',
    config = function()
        local capabilities = require('cmp_nvim_lsp').default_capabilities()
        local lspconfig = require('lspconfig')
        lspconfig.pyright.setup({
            capabilities = capabilities
        })
        lspconfig.clangd.setup({
            capabilities = capabilities
        })
        lspconfig.rust_analyzer.setup({
            capabilities = capabilities
        })
        lspconfig.texlab.setup({
            capabilities = capabilities
        })
        vim.cmd [[autocmd BufWritePre * lua vim.lsp.buf.format()]]

        vim.api.nvim_create_autocmd('LspAttach', {
            group = vim.api.nvim_create_augroup('UserLspConfig', {}),
            callback = function(ev)
                local opts = { buffer = ev.buf }
                vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
                vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
                vim.keymap.set('n', 'gh', vim.lsp.buf.hover, opts)
                vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
                vim.keymap.set('n', 'gn', vim.lsp.buf.rename, opts)
                vim.keymap.set({ 'n', 'v' }, 'gca', vim.lsp.buf.code_action, opts)
            end,
        })
    end,
}
