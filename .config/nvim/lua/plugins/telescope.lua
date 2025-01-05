return {
    'nvim-telescope/telescope.nvim',
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = function()
        require("telescope").setup({
            pickers = {
                find_files = {
                    theme = "dropdown",
                },
                buffers = {
                    theme = "dropdown",
                },
                live_grep = {
                    theme = "dropdown",
                },
                current_buffer_fuzzy_find = {
                    theme = "dropdown",
                },
                help_tags = {
                    theme = "dropdown",
                },
            },
            defaults = {
                scroll_strategy = 'limit',
                mappings = {
                    n = {
                        ["<C-c>"] = require('telescope.actions').close
                    },
                },
                vimgrep_arguments = require('globals').rg_base_command,
            },
        })
    end,
}
