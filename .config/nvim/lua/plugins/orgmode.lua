local org_dir = '~/Documents/org/'

return {
    'nvim-orgmode/orgmode',
    event = 'VeryLazy',
    ft = { 'org' },
    config = function()
        require('orgmode').setup({
            org_default_notes_file = org_dir .. 'tasks.org',
            org_agenda_files = org_dir .. 'tasks.org',
            org_startup_folded = 'content',
            org_todo_keywords = {'TODO(t)', 'DOING(g)', 'WAITING', 'DAY', 'EVENT(e)', 'REMINDER(r)', '|', 'DONE(d)'},

            org_todo_keyword_faces = {
                TODO = ':foreground red',
                DOING = ':foreground yellow',
                TODO = ':foreground green',
                WAITING = ':foreground pink',
                DAY = ':foreground blue',
                EVENT = ':foreground purple',
                REMINDER = ':foreground blue',
            },
            org_log_done = false,
            org_hide_leading_stars = true,
            org_tags_column = 0,
            mappings = {
                prefix = '<leader>c',
            },
            org_capture_templates = {
                t = 'todo',
                tt = {
                    description = 'todo',
                    template = '* TODO %?',
                },
                ti = {
                    description = 'timed TODO',
                    template = '* TODO %?\n SCHEDULED: %^t',
                },
            },
            org_agenda_custom_commands = {
                d = {
                    description = 'dashboard',
                    types = {
                        {
                            type = 'agenda',
                            org_agenda_overriding_header = 'My daily agenda',
                            org_agenda_span = 'day'
                        },
                        {
                            type = 'agenda',
                            org_agenda_overriding_header = 'Whole week overview',
                            org_agenda_span = 'week',
                            org_agenda_start_on_weekday = 1,
                            org_agenda_remove_tags = true
                        },
                    }
                }
            }
        })
    end,
}
