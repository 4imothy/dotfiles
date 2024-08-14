-- vim.cmd("source ~/Projects/dotfiles/.vim/treegrep.vim")

local notes_win = nil
function reset_notes_win()
    notes_win = nil
end

function float_notes()
    if notes_win then
        vim.api.nvim_win_close(notes_win, true)
        reset_notes_win()
    else
        local buf = vim.api.nvim_create_buf(false, false)
        local width = vim.api.nvim_get_option("columns")
        local height = vim.api.nvim_get_option("lines")
        local win_height = math.ceil(height * 0.8 - 4)
        local win_width = math.ceil(width * 0.8)
        local opts = {
            style = "minimal",
            relative = "editor",
            width = win_width,
            height = win_height,
            row = math.ceil((height - win_height) / 2 - 1),
            col = math.ceil((width - win_width) / 2),
            border = "rounded",
        }

        notes_win = vim.api.nvim_open_win(buf, true, opts)
        vim.api.nvim_buf_set_option(buf, "modifiable", true)
        vim.cmd("Neorg workspace notes")
        vim.cmd([[
        autocmd WinLeave <buffer> :lua reset_notes_win()
        autocmd WinLeave <buffer> :lua vim.api.nvim_buf_delete(]] .. buf .. [[, {force = true})
        ]])
    end
end

vim.opt.number = true
vim.opt.signcolumn = 'yes'
vim.opt.relativenumber = true
vim.opt.encoding= 'UTF-8'
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.tabstop = 4
vim.opt.wrap = false
vim.opt.hlsearch = true
vim.opt.splitbelow = true
vim.opt.clipboard = 'unnamedplus'
vim.opt.hidden = true
vim.opt.background = 'dark'
vim.opt.pumheight = 5
vim.opt.showmode = false
vim.opt.showcmd = false
vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.linebreak = true
vim.opt.laststatus = 3
vim.opt.scrolloff = 3
vim.opt.conceallevel = 0
vim.opt.shortmess:append 'I'
vim.opt.shortmess:append 'c'
vim.opt.swapfile = false
vim.opt.undofile = true
vim.opt.undodir = vim.fn.stdpath('data') .. '/undo'
vim.opt.undolevels = 1000
vim.opt.undoreload = 10000
vim.opt.guicursor = 'a:block,i:hor1'
vim.opt.foldlevelstart = 99
vim.opt.timeout = false

vim.g.mapleader = ','
vim.g.tex_flavor = 'tex'

local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(vim.env.LAZY or lazypath)

local signs = { Error = "󰅚", Warn = "󰀪", Hint = "󰌶", Info = "" }
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

require('lazy').setup('plugins', {
    checker = { enabled = false },
    change_detection = { notify = false },
})

-- vim.api.nvim_create_autocmd("VimEnter", {
-- 	callback = vim.schedule_wrap(function(data)
-- 		if data.file == "" or vim.fn.isdirectory(data.file) ~= 0 then
--             require("oil").open()
--             local oil_buf = vim.api.nvim_get_current_buf()
--             vim.cmd('enew')
--             vim.cmd('b' .. oil_buf)
-- 		end
-- 	end),
-- })

local function setkey(mode, l, r, opts)
    vim.keymap.set(mode, l, r, opts)
end

_G.diagnostics_enabled = true

function _G.toggle_diagnostics()
  if _G.diagnostics_enabled then
    vim.diagnostic.disable()
    print("Diagnostics disabled")
  else
    vim.diagnostic.enable()
    print("Diagnostics enabled")
  end
  _G.diagnostics_enabled = not _G.diagnostics_enabled
end

setkey('n', '<leader>n', vim.cmd.bnext)
setkey('n', '<leader>p', vim.cmd.bprevious)
setkey('n', '<leader>x', vim.cmd.bdelete)
setkey('n', '<leader>l', toggle_diagnostics)
setkey('n', '<leader>u', vim.cmd.UndotreeToggle)
setkey('n', '<leader>\\', vim.cmd.nohlsearch)
setkey('n', '<leader>e', function() require('telescope.builtin').find_files( { find_command = require('rg').files_command } ) end )
setkey('n', '<leader>f', require('telescope.builtin').live_grep)
setkey('n', '<leader>b', require('telescope.builtin').buffers)
setkey('n', '<leader>/', require('telescope.builtin').current_buffer_fuzzy_find)
setkey("n", "<leader>-", require('oil').open_float)

vim.api.nvim_create_autocmd('BufWritePre', {
    group = vim.api.nvim_create_augroup('trim_trailing_whitespace', {}),
    callback = function()
        local win_view = vim.fn.winsaveview()
        vim.cmd([[keeppatterns %s/\s\+$//e]])
        vim.fn.winrestview(win_view)
    end
})

vim.api.nvim_create_autocmd({"VimEnter", "VimResized"}, {
    pattern = { "tex", "txt", "markdown", "norg" },
    callback = function()
        if vim.o.columns - vim.o.numberwidth >= 78 then
            vim.o.textwidth = 78
        else
            vim.o.textwidth = vim.o.columns - vim.o.numberwidth
        end
    end
})

vim.api.nvim_create_autocmd("FileType", {
    pattern = { "tex", "txt", "markdown", "norg" },
    callback = function()
        vim.opt.spell = true
        vim.opt.spelllang:append("en_us", "en_gb")
        vim.opt.wrap = true
        local opts = { noremap = true, buffer = true }
        vim.keymap.set({ 'n', 'v' }, 'j', 'gj', opts)
        vim.keymap.set({ 'n', 'v' }, 'k', 'gk', opts)
    end
})

vim.api.nvim_create_autocmd('TermOpen', {
    callback = function()
        vim.opt.spell = false
    end
})

local spell_path = vim.fn.stdpath('config') .. '/spell'

for _, spell_file in ipairs(vim.fn.glob(spell_path .. '/*.add', 1, 1)) do
  if vim.fn.filereadable(spell_file) and (
      not vim.fn.filereadable(spell_file .. '.spl') or
      vim.fn.getftime(spell_file) > vim.fn.getftime(spell_file .. '.spl')
  ) then
    vim.cmd('silent exec "mkspell! " .. fnameescape("' .. spell_file .. '")')
  end
end
