return {
    "nvim-neorg/neorg",
    dependencies = { "nvim-lua/plenary.nvim", "luarocks.nvim" },
    config = function()
        require("neorg").setup {
            load = {
                ["core.defaults"] = {},
                ["core.ui.calendar"] = {},
                ["core.concealer"] = {
                    config = {
                        icons = {
                            todo = {
                                undone = {
                                    icon = " ",
                                },
                                pending = {
                                    icon = "-",
                                },
                                done = {
                                    icon = "x",
                                },
                            },
                        }
                    }
                },
                ["core.qol.todo_items"] = {
                    config = {
                        order = {
                            { "undone", " " },
                            { "pending", "-" },
                            { "done", "x" },
                        },
                    }
                },
                ["core.dirman"] = {
                    config = {
                        workspaces = {
                            notes = "~/Documents/notes"
                        },
                    },
                },
                ["core.completion"] = {
                    config = {
                        engine = "nvim-cmp",
                    },
                },
                ["core.esupports.indent"] = {
                    config = {
                        format_on_escape = false
                    },
                },
                ["core.keybinds"] = {
                    config = {
                        neorg_leader = "<leader>c",
                        hook = function(keybinds)
                            keybinds.remap_event("norg", "n", keybinds.leader .. "j", "core.integrations.treesitter.next.heading")
                            keybinds.remap_event("norg", "n", keybinds.leader .. "k", "core.integrations.treesitter.previous.heading")
                        end,
                    },
                }
            }
        }
    end
}
