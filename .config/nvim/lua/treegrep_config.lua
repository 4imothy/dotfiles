require('treegrep').setup({
    selection_file = '/tmp/tgrep-select',
    repeat_file = '/tmp/tgrep-repeat',
})
vim.keymap.set('n', '<leader>tt', function() require('treegrep').tgrep_with('--menu --live') end)
vim.keymap.set('n', '<leader>tr', function() require('treegrep').tgrep_with('--repeat --select --live') end)
vim.keymap.set('n', '<leader>tf', function() require('treegrep').tgrep_with('--files --select --live') end)
