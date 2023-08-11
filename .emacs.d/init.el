;;; init.el --- Timothy's cool init file
;;; Commentary:
;; Nice customizations for Emacs.

;; TODO change to eglot, lsp-mode is slow i think

;;; Code:
;; useful for quickly debugging emacs
;; (setq debug-on-error t)

;; make fullscreen and edit menu items
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 10)
(tooltip-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(setq inhibit-startup-screen t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
;; start *scratch* with no message and not as a Lisp Interactiobn
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "")

;; font
(set-face-attribute 'default nil :font "mononoki" :height 200)

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

;; Dired
;; from: https://www.emacswiki.org/emacs/DiredSortDirectoriesFirst
(defun my/dired-sort ()
  "Sort Dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  "Sort Dired listings with directories first before adding mark."
  (my/dired-sort))

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

;; fix error: `ls does not support --dired`
(when (string= system-type "darwin")
  (defvar dired-use-ls-dired nil))

;; keybindings
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; packages
(defvar package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(require 'package)

(package-initialize)
;; initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; theme
(use-package solarized-theme
  :custom
  (solarized-use-variable-pitch nil) ; don't change the fonts
  :config
  (load-theme 'solarized-light t))

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
  :custom
  (which-key-idle-delay 0.2)
  :config
  (which-key-mode 1))

;; modeline have to M-x nerd-icons-install-fonts to get icons
(use-package doom-modeline
  :custom ((doom-modeline-height 20))
  :config (doom-modeline-mode 1))

;; magit
(use-package magit)

(use-package pdf-tools)
;; run a M-x pdf-tools-install
;; PDF preview
;; to install latex:
;; - Install basictex
;; - sudo tlmgr option repository https://mirrors.rit.edu/CTAN/systems/texlive/tlnet/
;; - sudo tlmgr update –self
;; -  If needed: sudo tlmgr install <your_package_name>
;; -  sudo tlmgr install dvisvgm # for math preview
;; other dependencies
;; - ghostscript

;; org-mode
(use-package org
  :hook
  (org-mode . org-indent-mode)
  (org-mode . my/org-bindings)
  :bind
  ("C-c c" . org-capture)
  ("C-c a" . my/open-agenda)
  :custom
  (org-ellipsis "⤵")
  (org-agenda-files (list "~/Documents/org/"))
  (org-todo-keywords
   (quote ((sequence "TODO(t)" "|" "DOING(g)" "|" "DONE(d)" "|" "EVENT(e)"))))
  (org-agenda-span 14)
  (org-startup-with-latex-preview t)
  (org-directory "~/Documents/org")
  (org-columns-default-format "%ALLTAGS %TODO %30ITEM %22SCHEDULED %22DEADLINE")
  (org-default-notes-file (concat org-directory "/captures.org"))
  (org-agenda-custom-commands
   '(("d" "Dashboard"
      ((todo "TODO"
             ((org-agenda-overriding-header "todos")))
       (todo "DOING"
             ((org-agenda-overriding-header "doings")))
       (todo "DONE"
             ((org-agenda-overriding-header "dones")))
       (todo "EVENT"
             ((org-agenda-overriding-header "events")))
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
  :config
  (set-face-underline 'org-ellipsis nil)
  (add-hook 'org-after-todo-statistics-hook #'my/org-summary-todo-cookie)
  (add-hook 'org-checkbox-statistics-hook #'my/org-summary-checkbox-cookie)
  (defun my/open-agenda ()
    "Open the agenda and switch to the \"org-agenda-columns\" view."
    (interactive)
    (org-agenda)
    (defvar org-agenda-columns-active nil)
    (org-agenda-columns))

  (defvar solarized-green "#859900")
  (defvar solarized-red "#dc322f")
  ;; from this question: https://emacs.stackexchange.com/questions/7375/can-i-format-cells-in-an-org-mode-table-differently-depending-on-a-formula
  ;; and this person: https://emacs.stackexchange.com/users/15307/erki-der-loony
  (defface positive-face
    `((t :foreground ,solarized-green))
    "Indicates something positive.")

  (defface negative-face
    `((t :foreground ,solarized-red))
    "Indicates something negative.")

  (defun my/match-positive-numbers (limit)
    "Match positive numbers: LIMIT: buffer position to stop at."
    (let (result)
      (while
          (progn
            (when (looking-back "|" 1)
              (backward-char))
            (setq result (re-search-forward "| *\\([0-9\\., ]+\\) *|" limit t))
            (save-match-data
              (and result (not (looking-back "^ *|.*" 1))))))
      result))

  (defun my/match-negative-numbers (limit)
    "Match positive numbers: LIMIT: buffer position to stop at."
    (let (result)
      (while
          (progn
            (when (looking-back "|" 1)
              (backward-char))
            (setq result (re-search-forward "| *\\(- *[0-9\\., ]+\\) *|" limit t))
            (save-match-data
              (and result (not (looking-back "^ *|.*" 1))))))
      result))

  (font-lock-add-keywords 'org-mode
                          '((my/match-positive-numbers 1 'positive-face t))
                          'append)

  (font-lock-add-keywords 'org-mode
                          '((my/match-negative-numbers 1 'negative-face t))
                          'append)

  ;; list config
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Credit: Christian Tietze
  ;; https://christiantietze.de/posts/2021/02/emacs-org-todo-doing-done-checkbox-cycling/
  (defun org-todo-if-needed (state)
    "Change the TODO state of the current entry to STATE."
    (unless (string-equal (org-get-todo-state) state)
      (org-todo state)))

  (defun my/org-summary-todo-cookie (n-done n-not-done)
    "Update the TODO state of the entry based on the number of tasks done.

   N-DONE: Number of tasks marked as done.
   N-NOT-DONE: Number of tasks not marked as done."
    (let ((org-log-done nil)
          (org-log-states nil)) ; turn off logging
      (org-todo-if-needed
       (cond ((= n-done 0) "TODO")
             ((= n-not-done 0) "DONE")
             (t "DOING")))))

  (defun my/org-summary-checkbox-cookie ()
    "Update the TODO state of the current entry based on checkbox statistics."
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
  (defvar org-capture-templates
    '(("t"
       "Todo List Item"
       entry
       (file org-default-notes-file)
       "* TODO %?\n %i\n %a")))

  (defun my/org-bindings ()
    (local-set-key (kbd "C-c p") 'my/org-export-to-pdf-and-open)
    )
  ;; PDF previews
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
  )

;; open dashboard on startup
(add-hook 'emacs-startup-hook (lambda () (execute-kbd-macro (read-kbd-macro "C-c a d"))))

;; math preview
(use-package org-fragtog
  :hook
  (org-mode . org-fragtog-mode)
  :config
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-preview-latex-image-directory (concat user-emacs-directory "ltximg/"))
  )

;; Terminal
;; Need to install /glibtool/ and /cmake/
(use-package vterm
  :bind ("C-c t" . vterm)
  )

;; coding
;; compiling
(setq compile-command nil)

;; lsp
(use-package lsp-mode
  ;; TODO make this :hook like the website https://emacs-lsp.github.io/lsp-mode/page/installation/
  :hook
  (rust-mode . lsp)
  (elisp-mode . lsp)
  (c-mode . lsp)
  (js-mode . lsp)
  (python-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (lsp-completion-provider :none)
  (lsp-diagnostics-provider :flymake)
  (lsp-enable-snippet nil)
  :commands (lsp lsp-deferred)
  )

;; syntax reports
(use-package flymake
  :hook
  (emacs-lisp-mode . flymake-mode)
  :bind (("C-c f d"   . flymake-show-buffer-diagnostics)
         ("C-c f D" . flymake-show-project-diagnostics))
  :hook
  (lsp-mode . (lambda () (flymake-mode t)))
  (after-save-hook . my-flymake-refresh-errors)
  :custom
  (defun my-flymake-refresh-errors ()
    "Restart \"flymake\" to refresh reporting."
    (when flymake-mode
      (flymake-mode 0)
      (flymake-mode 1))
    )
)

;; completions
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert)
  :config
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

;; python
;; 1. *Command:* /pip3 install python-lsp-server[all]/
;; 2. put the pylsp in path
(use-package python-mode)
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
  :hook
  (rust-mode .
          (lambda () (setq indent-tabs-mode nil)))
  :init
  (defvar rust-format-on-save t))

;; JavaScript
;; - npm install -g typescript typescript-language-server
;; - put the typescript-language-server in /usr/local/bin
;; - to use create jsconfig.json or tsconfig.json
(use-package prettier-js
  :hook
  (js-mode . prettier-js-mode)
  )

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" default))
 '(package-selected-packages
   '(vterm prettier-js rust-mode pyvenv python-mode corfu lsp-mode flycheck org-fragtog pdf-tools magit doom-modeline which-key counsel swiper solarized-theme)))

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
 '(org-level-8 ((t (:inherit outline-8 :height 1.0))))
 '(org-table ((t (:foreground "#657b83")))))

;;; init.el ends here
