return {
    "folke/trouble.nvim",
    opts = {
        fold_open = "v",
        fold_closed = ">",
        indent_lines = false,
        use_diagnostic_signs = true
    },
    keys = {
        {
            "<leader>td",
            "<cmd>Trouble diagnostics toggle focus=true<cr>",
        },
        {
            "<leader>tD",
            "<cmd>Trouble diagnostics toggle filter.buf=0 focus=true<cr>",
        },
        {
            "<leader>ts",
            "<cmd>Trouble symbols toggle focus=true<cr>",
        },
    }
}
