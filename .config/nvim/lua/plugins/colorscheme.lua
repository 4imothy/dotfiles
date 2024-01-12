return {
    "sainnhe/everforest",
    lazy = false,
    priority = 1000,
    config = function()
        vim.g.everforest_diagnostic_virtual_text = 'colored'
        vim.g.everforest_background = 'soft'
        vim.cmd.colorscheme('everforest')
    end,
}
