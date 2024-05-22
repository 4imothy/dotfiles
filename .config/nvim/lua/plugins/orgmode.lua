local org_dir = '~/Documents/org/'

-- TODO tag options, rewrite all the other notes stuff in org
-- TODO want to change the line format in org agenda
return {
    'nvim-orgmode/orgmode',
    event = 'VeryLazy',
    enabled = false,
    ft = { 'org' },
    config = function()
        require('orgmode').setup({
            org_agenda_files = '~/Documents/org/**',
            org_default_notes_file = org_dir .. 'todos.org',
            org_startup_folded = 'content',
            org_todo_keywords = {'TODO(t)', 'DOING(g)', '|', 'DONE(d)'},
            org_todo_keyword_faces = {
                TODO = ':foreground blue',
                DOING = ':foreground cyan', -- TODO this doesn't work unless we first open org mode from a capture
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
                {
                    description = 'Agenda for the week',
                    key = 'w',
                    command = 'agenda',
                    args = { span = 'week' }
                },
            }
        })
    end,
}
