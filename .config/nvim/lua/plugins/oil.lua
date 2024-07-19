return {
    'stevearc/oil.nvim',
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
        require("oil").setup({
            default_file_explorer = true,
            view_options = {
                show_hidden = true,
                cursorline = true
            },
            columns = {
                'permissions',
                'icon',
            },
            constrain_cursor = 'editable',
            win_options = {
                cursorline = true,
                spell = false,
            },
            float = {
                padding = 10,
                max_width = 0,
                max_height = 0,
                border = 'rounded',
                win_options = {
                    winblend = 0,
                }
            }
        })
    end
}
