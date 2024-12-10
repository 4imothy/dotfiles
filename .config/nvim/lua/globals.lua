local M = {}

M.rg_base_command = {
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

M.rg_files_command = {}
for _, arg in ipairs(M.rg_base_command) do
    table.insert(M.rg_files_command, arg)
end
table.insert(M.rg_files_command, "--files")

M.text_file_types = { "tex", "txt", "markdown", "norg", "text" }

return M
