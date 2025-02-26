return {
    "nvim-neorg/neorg",
    dependencies = { "nvim-lua/plenary.nvim", "luarocks.nvim" },
    config = function()
        require("neorg").setup {
            load = {
                ["core.defaults"] = {},
                ["core.ui.calendar"] = {},
                ["core.concealer"] = {},
                ["core.dirman"] = {
                    config = {
                        workspaces = {
                            notes = "~/Documents/notes"
                        },
                    },
                },
                -- ["core.completion"] = {
                --     config = {
                --         engine = "blink",
                --     },
                -- },
                ["core.esupports.indent"] = {
                    config = {
                        format_on_escape = false
                    },
                },
            }
        }
    end
}
