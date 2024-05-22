return {
    "L3MON4D3/LuaSnip",
    build = "make install_jsregexp",
    config = function()
        require("luasnip.loaders.from_snipmate").lazy_load({ paths = { vim.fn.stdpath("config") .. "/snippets"} })
    end
}
