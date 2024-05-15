return {
    'nvim-lualine/lualine.nvim',
    config = function()
        local function diff_source()
            local gitsigns = vim.b.gitsigns_status_dict
            if gitsigns then
                return {
                    added = gitsigns.added,
                    modified = gitsigns.changed,
                    removed = gitsigns.removed
                }
            end
            return 'diff'
        end

        require('lualine').setup({
            options = {
                icons_enabled = false,
                use_mode_colors = false,
                component_separators = { left = '', right = ''},
                section_separators = { left = '', right = ''},
                -- component_separators = { left = '', right = ''},
                -- section_separators = { left = '', right = ''},
            },
            sections = {
                lualine_a = {},
                lualine_b = {'b:gitsigns_head', {'diff', source = diff_source}},
                lualine_c = {{'filename', path = 1}},
                lualine_x = {'encoding', 'filetype'},
                lualine_y = {'progress'},
                lualine_z = {}
            },
            tabline = {
                lualine_a = {
                    {
                        'buffers',
                        symbols = {
                            modified = '[+]',
                            alternate_file = '',
                            directory = '',
                        },
                    }
                },
                lualine_b = {},
                lualine_c = {},
                lualine_x = {},
                lualine_y = {},
                lualine_z = {'tabs'},
            },
        })
    end
}
