return {
    'nvim-lualine/lualine.nvim',
    config = function()
        require('lualine').setup({
            options = {
                icons_enabled = false,
                use_mode_colors = false,
                component_separators = { left = '', right = ''},
                section_separators = { left = '', right = ''},
            },
            sections = {
                lualine_a = {},
                lualine_b = {'FugitiveHead', 'diff', 'diagnostics'},
                lualine_c = {'filename'},
                lualine_x = {'encoding', 'filetype'},
                lualine_y = {'progress'},
                lualine_z = {}
            },
            tabline = {
                lualine_a = {{'buffers',
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
