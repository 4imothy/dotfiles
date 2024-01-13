return {
    'nvim-telescope/telescope.nvim', tag = '0.1.5',
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = function()
        require("telescope").setup({
            pickers = {
                find_files = {
                    theme = "dropdown",
                },
                live_grep = {
                    theme = "dropdown",
                },
                help_tags = {
                    theme = "dropdown",
                },
            },
            defaults = {
                scroll_strategy = 'limit',
                mappings = {
                    i = {
                        ["<Esc>"] = require('telescope.actions').close,
                    },
                },
                vimgrep_arguments = require('rg').base_command,
            },
        })
    end,
}
