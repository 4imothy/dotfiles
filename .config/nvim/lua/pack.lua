local gh = 'https://github.com/'

-- vim.opt.rtp:prepend(vim.fn.expand('~/Projects/treegrep'))
-- require('treegrep').build_tgrep()

local hooks = vim.api.nvim_create_autocmd({'PackChanged'}, {
    callback = function(ev)
        local active, name, kind = ev.data.active, ev.data.spec.name, ev.data.kind

        if name == 'treegrep' and (kind == 'install' or kind == 'update') then
            if not active then
                vim.cmd.packadd('treegrep')
            end
            require('treegrep').build_tgrep()
        end
    end,
})

vim.pack.add({
    gh .. 'nvim-lua/plenary.nvim',
    gh .. 'nvim-tree/nvim-web-devicons',
    gh .. '4imothy/treegrep',

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
    gh .. 'folke/trouble.nvim',

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
-- vim.lsp.enable('ltex')
vim.lsp.config['rust_analyzer'] = {
    capabilities = capabilities,
    on_attach = on_attach,
    settings = {
        ["rust-analyzer"] = {
            documentSymbol = {
                hierarchical = true,
            },
        },
    },
}

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

local parsers = {
    { 'cpp',        gh .. 'tree-sitter/tree-sitter-cpp' },
    { 'cmake',      gh .. 'uyha/tree-sitter-cmake' },
    { 'html',       gh .. 'tree-sitter/tree-sitter-html' },
    { 'css',        gh .. 'tree-sitter/tree-sitter-css' },
    { 'go',         gh .. 'tree-sitter/tree-sitter-go' },
    { 'rust',       gh .. 'tree-sitter/tree-sitter-rust' },
    { 'json',       gh .. 'tree-sitter/tree-sitter-json' },
    { 'yaml',       gh .. 'tree-sitter-grammars/tree-sitter-yaml' },
    { 'javascript', gh .. 'tree-sitter/tree-sitter-javascript' },
    { 'python',     gh .. 'tree-sitter/tree-sitter-python' },
    { 'typst',      gh .. 'uben0/tree-sitter-typst' },
    { 'latex',      gh .. 'latex-lsp/tree-sitter-latex' },
    { 'haskell',    gh .. 'tree-sitter/tree-sitter-haskell' },
    { 'comment',    gh .. 'stsewd/tree-sitter-comment' },
    { 'sql',        gh .. 'derekstride/tree-sitter-sql' },
    { 'kdl',        gh .. 'amaanq/tree-sitter-kdl' },
    { 'wgsl',       gh .. 'szebniok/tree-sitter-wgsl' },
}

local parser_dir = vim.fn.stdpath('data') .. '/site/parser'
local queries_dir = vim.fn.stdpath('data') .. '/site/queries'
vim.fn.mkdir(parser_dir, 'p')
vim.opt.rtp:append(vim.fn.stdpath('data') .. '/site')

local function install_parser(lang, repo)
    local out = parser_dir .. '/' .. lang .. '.so'
    if vim.fn.filereadable(out) == 1 then return end
    vim.notify('Installing parser: ' .. lang, vim.log.levels.INFO)
    local tmp = vim.fn.tempname()
    vim.fn.system({ 'git', 'clone', '--depth', '1', repo, tmp })
    vim.fn.system({ 'tree-sitter', 'build', '--output', out, tmp })
    local src = tmp .. '/queries'
    if vim.fn.isdirectory(src) == 1 then
        local dst = queries_dir .. '/' .. lang
        vim.fn.mkdir(dst, 'p')
        vim.fn.system({ 'cp', '-r', src .. '/.', dst })
    end
    vim.fn.delete(tmp, 'rf')
end

vim.api.nvim_create_user_command('TSInstallAll', function()
    for _, p in ipairs(parsers) do
        install_parser(p[1], p[2])
    end
    vim.notify('All parsers installed', vim.log.levels.INFO)
end, { desc = 'Install all treesitter parsers' })

local max_filesize = 100 * 1024
vim.api.nvim_create_autocmd('FileType', {
    callback = function(ev)
        local stats = vim.uv.fs_stat(vim.api.nvim_buf_get_name(ev.buf))
        if not stats or stats.size <= max_filesize then
            pcall(vim.treesitter.start)
        end
    end,
})

local todo_file = vim.env.TODO_FILE or vim.fn.expand('~/org/tasks.org')
local org_dir = vim.fn.fnamemodify(todo_file, ':h') .. '/'

local flavor = dark_mode_status == 'on' and 'frappe' or 'latte'
local ok_pal, palettes = pcall(require, 'catppuccin.palettes')
local p = ok_pal and palettes.get_palette(flavor) or {}
local function face(color)
    return color and (':foreground ' .. color .. ' :weight bold') or ''
end

require('orgmode').setup({
    org_default_notes_file = todo_file,
    org_agenda_files = todo_file,
    org_startup_folded = 'overview',
    org_startup_indented = true,
    org_todo_keywords = { 'TODO(t)', 'DAY', 'REMINDER(r)', '|', 'DONE(d)' },
    org_todo_keyword_faces = {
        TODO = face(p.red),
        DAY = face(p.blue),
        REMINDER = face(p.blue),
    },
    org_log_done = false,
    org_hide_leading_stars = true,
    org_hide_emphasis_markers = true,
    org_ellipsis = '⤵',
    org_tags_column = 0,
    org_agenda_remove_tags = false,
    ui = { folds = { colored = true } },
    mappings = { prefix = '<leader>c' },
    org_capture_templates = {
        t = { description = 'Todo', template = '* TODO %?' },
        e = { description = 'Event', template = '* EVENT %?' },
        i = { description = 'Timed todo', template = '* TODO %?\n DEADLINE: %^t' },
    },
    org_agenda_custom_commands = {
        d = {
            description = 'Dashboard',
            types = {
                { type = 'tags', match = '/DAY', org_agenda_overriding_header = 'Day' },
                { type = 'agenda', org_agenda_span = 'day', org_agenda_overriding_header = 'Today' },
                { type = 'tags', match = '/TODO|DOING', org_agenda_overriding_header = 'Tasks', org_agenda_sorting_strategy = { 'priority-down', 'time-up' } },
                { type = 'tags', match = '/WAITING', org_agenda_overriding_header = 'Waiting' },
                { type = 'tags', match = '/EVENT', org_agenda_overriding_header = 'Events' },
                { type = 'agenda', org_agenda_span = 10, org_agenda_start_day = '+1d', org_agenda_overriding_header = 'Upcoming' },
                { type = 'tags', match = '/REMINDER', org_agenda_overriding_header = 'Reminders' },
            },
        },
    },
})

require('orgmode.colors.highlights').define_highlights()

if p.blue then
    vim.api.nvim_set_hl(0, '@org.bold', { fg = p.blue, bold = true })
end

vim.keymap.set('n', '<leader>cf', function()
    require('telescope.builtin').find_files({
        cwd = org_dir,
        prompt_title = 'Org Files',
        find_command = { 'rg', '--files', '--max-depth', '1', '--glob', '*.org' },
    })
end, { desc = 'Find org file' })
