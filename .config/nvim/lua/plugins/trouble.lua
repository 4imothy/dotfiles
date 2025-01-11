return {
    'folke/trouble.nvim',
    opts = {},
    cmd = 'Trouble',
    keys = {
        {
            '<leader>td',
            '<cmd>Trouble diagnostics toggle focus=true<cr>',
        },
        {
            '<leader>tD',
            '<cmd>Trouble diagnostics toggle filter.buf=0 focus=true<cr>',
        },
        {
            '<leader>ts',
            '<cmd>Trouble symbols toggle focus=true win.position=bottom<cr>',
        },
        {
            '<leader>tl',
            '<cmd>Trouble lsp toggle focus=true<cr>',
        },
    }
}
