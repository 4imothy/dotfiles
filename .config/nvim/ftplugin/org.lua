vim.api.nvim_set_option_value("conceallevel", 3, { scope = 'local' })
vim.api.nvim_set_option_value("concealcursor", 'nc', { scope = 'local' })

vim.api.nvim_set_hl(0, 'OrgFolded', { bg = 'NONE' })
vim.opt_local.winhighlight = 'Folded:OrgFolded'

vim.keymap.set('n', '<leader>cv', function()
    local buf = vim.api.nvim_get_current_buf()
    if vim.b[buf].snacks_image_attached then
        pcall(vim.api.nvim_del_augroup_by_name, 'snacks.image.inline.' .. buf)
        require('snacks.image.placement').clean(buf)
        vim.b[buf].snacks_image_attached = nil
    else
        require('snacks.image.doc').attach(buf)
    end
end, { buffer = true, desc = 'Org: toggle inline images' })
