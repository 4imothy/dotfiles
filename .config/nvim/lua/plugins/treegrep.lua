return {
    '4imothy/treegrep',
    build = function()
        require('treegrep').build_tgrep()
    end,
    config = function()
        require('treegrep').setup({
            selection_file = '/tmp/tgrep-select',
            repeat_file = '/tmp/tgrep-repeat',
        })
        vim.keymap.set('n', '<leader>tt', function() require('treegrep').tgrep_with('--menu') end)
        vim.keymap.set('n', '<leader>tr', function() require('treegrep').tgrep_with('--repeat') end)
        vim.keymap.set('n', '<leader>tf', function() require('treegrep').tgrep_with('--files --select') end)
    end,
}
