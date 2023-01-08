# open in new tab
config.bind('t', 'set-cmd-text -s :open -t')
# hide statusbar
config.bind('xb', 'config-cycle statusbar.show always never')
# hide tabs
config.bind('xt', 'config-cycle tabs.show always never')
# hide both bar and tabs
config.bind('xx', 'config-cycle statusbar.show always never;; config-cycle tabs.show always never')
