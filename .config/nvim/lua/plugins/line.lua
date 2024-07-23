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
                icons_enabled = true,
                use_mode_colors = false,
                component_separators = { left = '', right = ''},
                section_separators = { left = '', right = ''},
                -- component_separators = { left = '', right = ''},
                -- section_separators = { left = '', right = ''},
            },
            sections = {
                lualine_a = {},
                -- lualine_a = {{
                --     'diagnostics',
                --     sources = { 'nvim_lsp', 'nvim_diagnostic', 'nvim_workspace_diagnostic'},
                --     diagnostics_color = {
                --         error = 'DiagnosticVirtualTextError',
                --         warn  = 'DiagnosticVirtualTextWarn',
                --         info  = 'DiagnosticVirtualTextInfo',
                --         hint  = 'DiagnosticVirtualTextHint',
                --     },
                --     colored = true,
                --     sections = { 'error', 'warn', 'info', 'hint' },
                --     symbols = {error = 'E', warn = 'W', info = 'I', hint = 'H'},
                -- }},
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
                        icons_enabled = false,
                        max_length = vim.o.columns * 2 / 3,
                        symbols = {
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
    end,
}
