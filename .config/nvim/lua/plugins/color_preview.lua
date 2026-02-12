  return {
    'echasnovski/mini.hipatterns',
    config = function()
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
              return {
                virt_text = { { '  ', hl } },
                virt_text_pos = 'inline',
              }
            end,
          },
        },
      })
    end,
  }
