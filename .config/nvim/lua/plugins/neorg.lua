return {
    "nvim-neorg/neorg",
    build = ":Neorg sync-parsers",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
        require("neorg").setup {
            load = {
                ["core.defaults"] = {},
                -- TODO uncomment on stable 0.10.0 ["core.ui.calendar"] = {},
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
                            main = "~/Documents/notes"
                        },
                    },
                },
                ["core.completion"] = {
                    config = {
                        engine = "nvim-cmp",
                    },
                },
                ["core.keybinds"] = {
                    config = {
                        neorg_leader = "<Leader>c",
                        hook = function(keybinds)
                            keybinds.remap_key("norg", "n", keybinds.leader .. "nn", keybinds.leader .. "n")
                            keybinds.remap_event("norg", "n", keybinds.leader .. "j", "core.integrations.treesitter.next.heading")
                            keybinds.remap_event("norg", "n", keybinds.leader .. "k", "core.integrations.treesitter.previous.heading")
                        end,
                    },
                }
            }
        }
    end
}
