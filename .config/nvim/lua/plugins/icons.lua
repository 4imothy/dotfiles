return {
    'nvim-tree/nvim-web-devicons',
    config = function()
        local devicons = require('nvim-web-devicons')
        local icons = devicons.get_icons()
        for icon_name, icon_data in pairs(icons) do
            icon_data.color = 'foreground'
        end
        devicons.set_icon(icons)
        devicons.setup()
    end
}
