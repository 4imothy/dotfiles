return {
    -- "sainnhe/everforest",
    -- lazy = false,
    -- priority = 1000,
    -- config = function()
    --     vim.g.everforest_diagnostic_virtual_text = 'colored'
    --     vim.g.everforest_background = 'soft'
    --     vim.cmd.colorscheme('everforest')
    -- end,
    -- "EdenEast/nightfox.nvim",
    "catppuccin/nvim",
    -- "rebelot/kanagawa.nvim",
    -- "rose-pine/neovim",
    lazy = false,
    priority = 1000,
    config = function()
        --     vim.cmd.colorscheme('terafox')
        -- vim.cmd.colorscheme('catppuccin-frappe')
        local dark_mode_status = vim.fn.system('dark-mode status'):gsub('%s+', '')
        if dark_mode_status == 'on' then
            vim.cmd.colorscheme('catppuccin-frappe')
        else
            vim.cmd.colorscheme('catppuccin-latte') -- Light mode alternative
        end
        -- vim.cmd.colorscheme('rose-pine-moon')
    end,
}
