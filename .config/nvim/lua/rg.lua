local M = {}

M.base_command = {
    "rg",
    "--color=never",
    "--no-heading",
    "--with-filename",
    "--line-number",
    "--column",
    "--smart-case",
    "--hidden",
    "--glob=!.git/"
}

M.files_command = {}
for _, arg in ipairs(M.base_command) do
    table.insert(M.files_command, arg)  -- Copy shared arguments
end
table.insert(M.files_command, "--files")  -- A

return M
