;; initialize package
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
;; initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; theme
(use-package solarized-theme
  :config
  (setq solarized-use-variable-pitch nil) ; don't change the fonts
  (load-theme 'solarized-light t))

;; make fullscreen and edit menu items
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
;; disable the menu bar
(menu-bar-mode -1)
;; disable the toolbar
(tool-bar-mode -1)
;; set fringe mode to provide some breathing room
(set-fringe-mode 10)
;; disable tooltips
(tooltip-mode -1)
;; remove scroll bars
(scroll-bar-mode -1)
;; disable the startup screen
(setq inhibit-startup-screen t)
;; disable audio beeps
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; font
(set-face-attribute 'default nil :font "mononoki" :height 200)

;; keybindings
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c t") 'vterm)
(global-set-key (kbd "C-c a") 'my/open-dashboard)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; tabs
(setq-default tab-width 4
              indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default python-indent-offset 4)

;; enable line and column numbers
(column-number-mode)

;; enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; make command prefixes show fast
(setq echo-keystrokes 0.01)

;; line wrapping
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; remove whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; suggestions
(use-package swiper)
(use-package ivy
  :diminish
  :bind ("C-s" . swiper)
  :init
  (ivy-mode 1))
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)))
(use-package which-key
  :init (which-key-mode)
  (setq which-key-idle-delay 0.2))

;; modeline have to M-x nerd-icons-install-fonts to get icons
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; magit
(use-package magit)

;; org-mode
 (use-package org
      :hook (org-mode . my/org-mode-setup)
      :config
      (set-face-underline 'org-ellipsis nil)
      (add-hook 'org-after-todo-statistics-hook #'my/org-summary-todo-cookie)
      (add-hook 'org-checkbox-statistics-hook #'my/org-summary-checkbox-cookie)
      (setq org-ellipsis "⤵")
      (setq org-agenda-files (list "~/Documents/org/"))
      (setq org-todo-keywords
            (quote ((sequence "TODO(t)" "|" "DOING(g)" "|" "DONE(d)"))))
      (setq org-agenda-span 14)
      (setq org-directory "~/Documents/org")
      (setq org-columns-default-format "%ALLTAGS %TODO %30ITEM %22SCHEDULED")
      (setq org-startup-with-latex-preview t)
      (setq org-default-notes-file (concat org-directory "/captures.org"))
      (setq org-agenda-custom-commands
            '(("d" "Dashboard"
               ((todo "TODO"
                      ((org-agenda-overriding-header "todos")))
                (todo "DOING"
                      ((org-agenda-overriding-header "doings")))
                (todo "DONE"
                      ((org-agenda-overriding-header "dones")))
                (agenda ""
                        ((org-agenda-span 1)
                         (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                         (org-agenda-format-date "%A %-e %B %Y")
                         (org-agenda-overriding-header "day")
                         ))
                (agenda ""
                        ((org-agenda-start-day nil)
                         (org-agenda-start-day "+1d")
                         (org-agenda-span 10)
                         (org-agenda-day-face-function (lambda (date) '(:underline t :inherit org-agenda-date)))
                         (org-agenda-overriding-header "ten days out")
                         )))
               ((org-agenda-window-setup 'only-window)))))
      )


(defun my/org-mode-setup ()
  (org-indent-mode))

(defun my/open-dashboard ()
  "Open the agenda and switch to the org-agenda-columns view."
  (interactive)
  (org-agenda)
  (setq org-agenda-columns-active nil) ;; TODO see what this does
  (org-agenda-columns))

;; heading sizes
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.25))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.15))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.1))))
 '(org-level-6 ((t (:inherit outline-6 :height 1.05))))
 '(org-level-7 ((t (:inherit outline-7 :height 1.0))))
 '(org-level-8 ((t (:inherit outline-8 :height 1.0)))))

;; list config
;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Credit: Christian Tietze
;; https://christiantietze.de/posts/2021/02/emacs-org-todo-doing-done-checkbox-cycling/
(defun org-todo-if-needed (state)
  (unless (string-equal (org-get-todo-state) state)
    (org-todo state)))

(defun my/org-summary-todo-cookie (n-done n-not-done)
  (let ((org-log-done nil)
        (org-log-states nil)) ; turn off logging
    (org-todo-if-needed
     (cond ((= n-done 0) "TODO")
           ((= n-not-done 0) "DONE")
           (t "DOING")))))
(defun my/org-summary-checkbox-cookie ()
  (let ((beg (point))
        (end (save-excursion (end-of-line) (point))))
    (unless (not (org-get-todo-state))
      (save-excursion
        (org-back-to-heading t)
        (if (re-search-forward "\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
            (let ((num-done (match-string 1))
                  (num-total (match-string 2)))
              (org-todo-if-needed
               (cond ((equal num-done num-total) "DONE")
                     ((or (equal (string-trim num-done) "")
                          (equal num-done "0")) "TODO")
                     (t "DOING"))))
          (org-todo-if-needed "DOING"))))))

;; captures
(setq org-capture-templates
      '(("t"
         "Todo List Item"
         entry
         (file org-default-notes-file)
         "* TODO %?\n %i\n %a")))

;; PDF preview
;; to install latex:
;; - Install basictex
;; - sudo tlmgr option repository https://mirrors.rit.edu/CTAN/systems/texlive/tlnet/
;; - sudo tlmgr update –self
;; -  If needed: sudo tlmgr install <your_package_name>
;; -  sudo tlmgr install dvisvgm # for math preview
;; other dependencies
;; - ghostscript

(use-package pdf-tools)
;; run a M-x pdf-tools-install

(defun my/org-export-to-pdf-and-open ()
  "Export the current Org mode buffer to PDF and open it in a window to the right."
  (interactive)
  ;; Export to PDF
  (org-latex-export-to-pdf)

  ;; Define the PDF file name (assuming the same base name as the Org file)
  (let* ((org-file (buffer-file-name))
         (pdf-file (concat (file-name-sans-extension org-file) ".pdf")))

    ;; Open the PDF file in a window to the right
    (if (file-exists-p pdf-file)
        (progn
          (delete-other-windows)
          (split-window-horizontally)
          (other-window 1)
          (find-file pdf-file)
          (other-window 1))
      (message "PDF export failed."))))

;; Bind the function to a key combination if desired (e.g., C-c p)
(defun my/setup-org-preview ()
  "Setup for org-mode."
  (local-set-key (kbd "C-c p") 'my/org-export-to-pdf-and-open))

(add-hook 'org-mode-hook 'my/setup-org-preview)

;; math preview
(use-package org-fragtog
  :hook
  (org-mode . org-fragtog-mode)
  :config
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-preview-latex-image-directory (concat user-emacs-directory "ltximg/"))
  )

;; coding
;; compiling
(setq compile-command nil)

;; syntax reports
(use-package flycheck)

;; lsp
(use-package lsp-mode
  :init (add-hook 'rust-mode-hook #'lsp)
  :commands (lsp lsp-deferred)
  :hook
  (lsp-mode . efs/lsp-mode-setup)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; completions
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

;; python
;; 1. *Command:* /pip3 install python-lsp-server[all]/
;; 2. put the pylsp in path
(use-package python-mode
  :hook (python-mode . lsp-deferred))
(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

;; rust
;; - /curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh/ this placed file in ~/.cargo/
;; - To uninstall run: /rustup self uninstall/ from https://rust-lang.github.io/rustup/installation/index.html
;; - for lsp: /rustup component add rust-analyzer/
;; - locate its location with: rustup which rust-analyzer
;; - add that path to /$PATH/

;; add rust-analyzer to exec-path for lsp-mode
(add-to-list 'exec-path "~/.cargo/bin")
(use-package rust-mode
  :init
  (setq rust-format-on-save t))
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;; C
(add-hook 'c-mode-hook 'lsp)

;; JavaScript
;; - npm install -g typescript typescript-language-server
;; - put the typescript-language-server in /usr/local/bin
;; - to use create jsconfig.json or tsconfig.json
(add-hook 'js-mode-hook 'lsp)
(use-package prettier-js)
(add-hook 'js-mode-hook 'prettier-js-mode)

;; Other languages not setup yet
;; Markdown
;; - brew install marksman
;; - installed to /opt/homebrew/Cellar/marksman
;; Go
;; - brew install go
;; - go install golang.org/x/tools/gopls@latest
;; - add $HOME/bin/go to $PATH
;; wgsl
;; - cargo install --git https://github.com/wgsl-analyzer/wgsl-analyzer wgsl_analyzer
;; zig
;; - brew install zig
;; - LSP:
;; -brew install zls

;; Terminal
;; Need to install /glibtool/ and /cmake/
(use-package vterm)

;; yes-or-no -> y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; change backups location
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; open dashboard
(add-hook 'after-init-hook (lambda () (execute-kbd-macro (read-kbd-macro "C-c a d"))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(vterm prettier-js rust-mode pyvenv python-mode corfu lsp-ui lsp-mode flycheck org-fragtog pdf-tools magit doom-modeline which-key counsel swiper solarized-theme)))
