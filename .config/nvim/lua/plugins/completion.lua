return {
    'saghen/blink.cmp',
    version = '*',
    opts = {
        keymap = { preset = 'default' },
        completion = {
            menu = {
                max_height = 5,
                draw = {
                    components = {
                        label = {
                            ellipsis = true,
                            width = {
                                min = 20,
                                max = 20,
                            },
                        }
                    }
                }
            }
        },
        sources = {
            default = { 'snippets', 'lsp', 'path', 'buffer' },
            providers = {
                path = {
                    name = 'path',
                    module = 'blink.cmp.sources.path',
                    score_offset = 4
                },
                snippets = {
                    name = 'snippets',
                    module = 'blink.cmp.sources.snippets',
                    score_offset = 3
                },
                lsp = {
                    name = 'lsp',
                    module = 'blink.cmp.sources.lsp',
                    score_offset = 2
                },
                buffer = {
                    name = 'buffer',
                    module = 'blink.cmp.sources.buffer',
                    score_offset = 1,
                    min_keyword_length = 5,
                    transform_items = function (a, items)
                        local keyword = a.get_keyword()
                        local correct, case
                        if keyword:match('^%l') then
                            correct = '^%u%l+$'
                            case = string.lower
                        elseif keyword:match('^%u') then
                            correct = '^%l+$'
                            case = string.upper
                        else
                            return items
                        end

                        local seen = {}
                        local out = {}
                        for _, item in ipairs(items) do
                            local raw = item.insertText
                            if raw:match(correct) then
                                local text = case(raw:sub(1,1)) .. raw:sub(2)
                                item.insertText = text
                                item.label = text
                            end
                            if not seen[item.insertText] then
                                seen[item.insertText] = true
                                table.insert(out, item)
                            end
                        end
                        return out
                    end
                }
            }
        },
    },
}
