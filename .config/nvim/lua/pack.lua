local gh = 'https://github.com/'
vim.pack.add({
    gh .. 'nvim-lua/plenary.nvim',
    gh .. 'nvim-tree/nvim-web-devicons',

    { src = gh .. 'saghen/blink.cmp', version = 'v1' },

    gh .. 'neovim/nvim-lspconfig',
    gh .. 'SmiteshP/nvim-navic',

    gh .. 'catppuccin/nvim',
    gh .. 'nvim-lualine/lualine.nvim',

    gh .. 'numToStr/Comment.nvim',
    gh .. 'lewis6991/gitsigns.nvim',
    gh .. 'stevearc/oil.nvim',
    gh .. 'echasnovski/mini.hipatterns',

    gh .. 'nvim-telescope/telescope.nvim',
    gh .. '4imothy/treegrep',
    gh .. 'folke/trouble.nvim',

    { src = gh .. 'nvim-treesitter/nvim-treesitter', version = 'main' },

    gh .. 'nvim-orgmode/orgmode',
})

local dark_mode_status = vim.fn.system('dark-mode status'):gsub('%s+', '')
if dark_mode_status == 'on' then
    vim.cmd.colorscheme('catppuccin-frappe')
else
    vim.cmd.colorscheme('catppuccin-latte')
end

require('Comment').setup({
    toggler = { line = 'gcc', block = 'gbc' },
    opleader = { line = 'gc', block = 'gb' },
})

require('blink.cmp').setup({
    keymap = { preset = 'default' },
    completion = {
        menu = {
            max_height = 5,
            draw = {
                components = {
                    label = { ellipsis = true, width = { min = 20, max = 20 } },
                },
            },
        },
    },
    sources = {
        default = { 'snippets', 'lsp', 'path', 'buffer' },
    },
})

local devicons = require('nvim-web-devicons')
local icons = devicons.get_icons()
for _, icon_data in pairs(icons) do
    icon_data.color = 'foreground'
end
devicons.set_icon(icons)
devicons.setup()

require('gitsigns').setup({
    signcolumn = true,
    numhl = false,
    linehl = false,
    word_diff = false,
    watch_gitdir = { follow_files = true },
    auto_attach = true,
    attach_to_untracked = false,
    max_file_length = 40000,
    on_attach = function(bufnr)
        local gitsigns = require('gitsigns')
        local function map(mode, l, r, opts)
            opts = opts or {}
            opts.buffer = bufnr
            vim.keymap.set(mode, l, r, opts)
        end
        map('n', ']h', function()
            if vim.wo.diff then vim.cmd.normal({ ']h', bang = true })
            else gitsigns.nav_hunk('next') end
        end)
        map('n', '[h', function()
            if vim.wo.diff then vim.cmd.normal({ '[h', bang = true })
            else gitsigns.nav_hunk('prev') end
        end)
        map('n', '<leader>hs', gitsigns.stage_hunk)
        map('n', '<leader>hu', gitsigns.undo_stage_hunk)
        map('n', '<leader>hr', gitsigns.reset_hunk)
        map('n', '<leader>hp', gitsigns.preview_hunk)
        map('n', '<leader>hb', function() gitsigns.blame_line({ full = true }) end)
        map('n', '<leader>hd', gitsigns.diffthis)
        map('v', '<leader>hs', function() gitsigns.stage_hunk({ vim.fn.line('.'), vim.fn.line('v') }) end)
        map('v', '<leader>hr', function() gitsigns.reset_hunk({ vim.fn.line('.'), vim.fn.line('v') }) end)
    end,
})

local capabilities = require('blink.cmp').get_lsp_capabilities()
local navic = require('nvim-navic')
local on_attach = function(client, bufnr)
    if client.server_capabilities.documentSymbolProvider then
        navic.attach(client, bufnr)
    end
end
vim.lsp.config('*', { capabilities = capabilities, on_attach = on_attach })
vim.lsp.enable('pyright')
vim.lsp.enable('gopls')
vim.lsp.config['ts_ls'] = {
    cmd = { 'typescript-language-server', '--stdio' },
    filetypes = { 'typescript', 'typescriptreact', 'javascript', 'javascriptreact' },
    capabilities = capabilities,
    on_attach = on_attach,
}
vim.lsp.enable('ts_ls')
vim.lsp.enable('tinymist')
vim.lsp.enable('clangd')
vim.lsp.enable('rust_analyzer')
vim.lsp.enable('texlab')
vim.lsp.enable('hls')
vim.lsp.config['ltex'] = {
    filetypes = { 'bib', 'gitcommit', 'markdown', 'org', 'plaintex', 'tex', 'html', 'txt' },
    capabilities = capabilities,
    on_attach = on_attach,
    settings = {
        ltex = {
            language = 'auto',
            diagnosticSeverity = 'information',
            additionalRules = { languageModel = '~/Projects/dotfiles/ngrams/' },
            disabledRules = { ['en-US'] = { 'MORFOLOGIK_RULE_EN_US' } },
        },
    },
}
vim.lsp.enable('ltex')
vim.api.nvim_create_autocmd('LspAttach', {
    group = vim.api.nvim_create_augroup('UserLspConfig', {}),
    callback = function(ev)
        local opts = { buffer = ev.buf }
        vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
        vim.keymap.set('n', 'gn', vim.lsp.buf.rename, opts)
        vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
        vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
        vim.keymap.set({ 'n', 'v' }, 'ga', vim.lsp.buf.code_action, opts)
    end,
})

local function diff_source()
    local gitsigns = vim.b.gitsigns_status_dict
    if gitsigns then
        return { added = gitsigns.added, modified = gitsigns.changed, removed = gitsigns.removed }
    end
end
require('lualine').setup({
    options = {
        icons_enabled = true,
        use_mode_colors = false,
        component_separators = { left = '', right = '' },
        section_separators = { left = '', right = '' },
    },
    sections = {
        lualine_a = {},
        lualine_b = { 'b:gitsigns_head', { 'diff', source = diff_source } },
        lualine_c = { { 'filename', path = 1 }, { 'navic' } },
        lualine_x = { 'encoding', 'filetype' },
        lualine_y = { 'progress' },
        lualine_z = {},
    },
    tabline = {
        lualine_a = {{
            'buffers',
            icons_enabled = false,
            symbols = { alternate_file = '', directory = '' },
        }},
        lualine_b = {}, lualine_c = {}, lualine_x = {}, lualine_y = {},
        lualine_z = { 'tabs' },
    },
})

require('oil').setup({
    default_file_explorer = true,
    delete_to_trash = true,
    view_options = { show_hidden = true, cursorline = true },
    columns = { 'permissions', 'icon' },
    constrain_cursor = 'editable',
    win_options = { cursorline = true, spell = false },
    float = {
        padding = 5,
        max_width = math.floor(vim.o.columns * 0.6),
        max_height = 0,
        border = 'rounded',
        win_options = { winblend = 0 },
    },
})

require('telescope').setup({
    pickers = {
        find_files = { theme = 'dropdown' },
        buffers = { theme = 'dropdown' },
        live_grep = { theme = 'dropdown' },
        diagnostics = { theme = 'dropdown' },
        lsp_document_symbols = { theme = 'dropdown' },
        current_buffer_fuzzy_find = { theme = 'dropdown' },
        help_tags = { theme = 'dropdown' },
    },
    defaults = {
        scroll_strategy = 'limit',
        mappings = { n = { ['<C-c>'] = require('telescope.actions').close } },
        vimgrep_arguments = require('globals').rg_base_command,
    },
})

require('trouble').setup({})
vim.keymap.set('n', '<leader>td', '<cmd>Trouble diagnostics toggle focus=true<cr>')
vim.keymap.set('n', '<leader>tD', '<cmd>Trouble diagnostics toggle filter.buf=0 focus=true<cr>')
vim.keymap.set('n', '<leader>ts', '<cmd>Trouble symbols toggle focus=true win.position=bottom<cr>')
vim.keymap.set('n', '<leader>tl', '<cmd>Trouble lsp toggle focus=true<cr>')

vim.cmd('packadd nvim.undotree')

local hipatterns = require('mini.hipatterns')
hipatterns.setup({
    highlighters = {
        srgb_u8 = {
            pattern = 'Color::srgb_u8%(%s*%d+%s*,%s*%d+%s*,%s*%d+%s*%)()',
            group = '',
            extmark_opts = function(_, match, data)
                local r, g, b = data.full_match:match('Color::srgb_u8%(%s*(%d+)%s*,%s*(%d+)%s*,%s*(%d+)')
                if not r then return {} end
                local hex = string.format('#%02X%02X%02X', tonumber(r), tonumber(g), tonumber(b))
                local hl = hipatterns.compute_hex_color_group(hex, 'bg')
                return { virt_text = { { '  ', hl } }, virt_text_pos = 'inline' }
            end,
        },
    },
})

require('treegrep').setup({
    selection_file = '/tmp/tgrep-select',
    repeat_file = '/tmp/tgrep-repeat',
})
vim.keymap.set('n', '<leader>tt', function() require('treegrep').tgrep_with('--menu') end)
vim.keymap.set('n', '<leader>tr', function() require('treegrep').tgrep_with('--repeat --select') end)
vim.keymap.set('n', '<leader>tm', function() require('treegrep').tgrep_with('--menu --repeat') end)
vim.keymap.set('n', '<leader>tf', function() require('treegrep').tgrep_with('--files --select') end)
vim.api.nvim_create_autocmd('User', {
    pattern = 'PackChanged',
    once = true,
    callback = function(ev)
        if ev.data and ev.data.name == 'treegrep' and ev.data.kind == 'install' then
            require('treegrep').build_tgrep()
        end
    end,
})

local ts = require('nvim-treesitter')
if ts.install then
    ts.install({
        'lua', 'cpp', 'cmake', 'html', 'css', 'go', 'rust', 'json', 'yaml',
        'javascript', 'python', 'wgsl', 'typst', 'latex', 'haskell',
        'comment', 'kdl', 'sql',
    })
end
local max_filesize = 100 * 1024
vim.api.nvim_create_autocmd('FileType', {
    callback = function(ev)
        local stats = vim.uv.fs_stat(vim.api.nvim_buf_get_name(ev.buf))
        if not stats or stats.size <= max_filesize then
            pcall(vim.treesitter.start)
        end
    end,
})

local org_dir = '~/Documents/org/'
require('orgmode').setup({
    org_default_notes_file = org_dir .. 'tasks.org',
    org_agenda_files = org_dir .. 'tasks.org',
    org_startup_folded = 'content',
    org_todo_keywords = { 'TODO(t)', 'DOING(g)', 'WAITING', 'DAY', 'EVENT(e)', 'REMINDER(r)', '|', 'DONE(d)' },
    org_todo_keyword_faces = {
        TODO = ':foreground red',
        DOING = ':foreground yellow',
        WAITING = ':foreground pink',
        DAY = ':foreground blue',
        EVENT = ':foreground purple',
        REMINDER = ':foreground blue',
    },
    org_log_done = false,
    org_hide_leading_stars = true,
    org_tags_column = 0,
    mappings = { prefix = '<leader>c' },
    org_capture_templates = {
        t = 'todo',
        tt = { description = 'todo', template = '* TODO %?' },
        ti = { description = 'timed TODO', template = '* TODO %?\n SCHEDULED: %^t' },
    },
    org_agenda_custom_commands = {
        d = {
            description = 'dashboard',
            types = {
                { type = 'agenda', org_agenda_overriding_header = 'My daily agenda', org_agenda_span = 'day' },
                { type = 'agenda', org_agenda_overriding_header = 'Whole week overview', org_agenda_span = 'week', org_agenda_start_on_weekday = 1, org_agenda_remove_tags = true },
            },
        },
    },
})
