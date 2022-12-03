-- put relative numbers on side and actual on current line
vim.wo.relativenumber = true
vim.wo.number = true

-- make tabs go two spaces
vim.bo.expandtab = true;
vim.bo.shiftwidth = 2;
vim.bo.softtabstop = 2;

require('plugins')

