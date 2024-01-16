vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.encoding= 'UTF-8'
vim.opt.signcolumn = 'no'
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
vim.opt.shortmess:append 'c'
vim.opt.termguicolors = true
vim.opt.swapfile = false
vim.opt.undofile = true
vim.opt.undodir = vim.fn.stdpath('data') .. '/undo'
vim.opt.undolevels=1000
vim.opt.undoreload=10000
vim.opt.guicursor = 'a:block,i:hor10'

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

require('lazy').setup('plugins', {
    checker = { enabled = false },
    change_detection = { notify = false },
})

vim.keymap.set('n', '<leader>n', vim.cmd.bnext)
vim.keymap.set('n', '<leader>p', vim.cmd.bprevious)
vim.keymap.set('n', '<leader>x', vim.cmd.bdelete)
vim.keymap.set('n', '<leader>u', vim.cmd.UndotreeToggle)
vim.keymap.set('n', '<leader>e', function() require('telescope.builtin').find_files( { find_command = require('rg').files_command } ) end )
vim.keymap.set('n', '<leader>f', require('telescope.builtin').live_grep)
vim.keymap.set('n', '<leader>b', require('telescope.builtin').buffers)
vim.keymap.set('n', '<leader>/', require('telescope.builtin').current_buffer_fuzzy_find)
vim.keymap.set("n", "<leader>-", require('oil').open)
vim.keymap.set('n', '<leader>s', '<Plug>(easymotion-s)')
vim.keymap.set('n', '<leader>w', '<Plug>(easymotion-w)')
vim.keymap.set('n', '<leader>\\', vim.cmd.nohlsearch)
vim.api.nvim_set_keymap("i", "<Tab>", "vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<Tab>'", {expr = true})
vim.api.nvim_set_keymap("s", "<Tab>", "vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<Tab>'", {expr = true})

vim.api.nvim_create_autocmd('BufWritePre', {
  group = vim.api.nvim_create_augroup('trim_trailing_whitespace', {}),
  callback = function()
    local win_view = vim.fn.winsaveview()
    vim.cmd([[keeppatterns %s/\s\+$//e]])
    vim.fn.winrestview(win_view)
  end,
})

vim.api.nvim_create_autocmd("FileType", {
    pattern = { "tex", "txt", "markdown" },
    callback = function()
        vim.opt.spellcapcheck = ''
        vim.opt.spell = true
        vim.opt.linebreak = true
        vim.opt.wrap = true
        vim.opt.spelllang='en_us'
    end,
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

