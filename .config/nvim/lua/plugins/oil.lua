return {
    'stevearc/oil.nvim',
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
        local max_width = math.floor(vim.api.nvim_get_option("columns") * 0.6)
        require("oil").setup({
            default_file_explorer = true,
            delete_to_trash = true,
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
                padding = 5,
                max_width = max_width,
                max_height = 0,
                border = 'rounded',
                win_options = {
                    winblend = 0,
                }
            }
        })
    end
}
