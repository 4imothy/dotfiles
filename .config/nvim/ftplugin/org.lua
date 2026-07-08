vim.api.nvim_set_option_value("conceallevel", 3, { scope = 'local' })
vim.api.nvim_set_option_value("concealcursor", 'nc', { scope = 'local' })

vim.opt_local.number = true
vim.opt_local.wrap = true
vim.opt_local.linebreak = true

vim.api.nvim_set_hl(0, 'OrgFolded', { bg = 'NONE' })
vim.opt_local.winhighlight = 'Folded:OrgFolded'
