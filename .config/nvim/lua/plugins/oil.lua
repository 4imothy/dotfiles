return {
    'stevearc/oil.nvim',
    opts = function(_, opts)
        opts.default_file_explorer = true
        opts.view_options = {
            show_hidden = true,
            cursorline = true
        }
        opts.columns = {
            'permissions',
        }
        opts.win_options = {
            cursorline = true,
        }
        constrain_cursor = "editable"
    end
}
