;;; init.el --- Timothy's cool init file
;;; Commentary:
;; Nice customizations for Emacs.

;;; Code:
;; useful for quickly debugging emacs
;; (setq debug-on-error t)

;; packages
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq straight-check-for-modifications nil)

(setq treesit-language-source-alist
      '((typst "https://github.com/uben0/tree-sitter-typst")
        (python "https://github.com/tree-sitter/tree-sitter-python")))
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))

(dolist (lang (mapcar #'car treesit-language-source-alist))
  (unless (treesit-language-available-p lang)
    (treesit-install-language-grammar lang)))

(use-package emacs
  :hook
  ((window-setup-hook . toggle-frame-maximized)
   (before-save-hook . add-newline-at-end-if-missing)
   (before-save-hook . whitespace-cleanup)
   (text-mode-hook . (lambda () (display-line-numbers-mode 1) (visual-line-mode)))
   (prog-mode-hook . (lambda () (display-line-numbers-mode 1) (visual-line-mode)))
   (org-mode-hook . (lambda () (display-line-numbers-mode 1) (visual-line-mode)))
   (conf-mode-hook . (lambda () (display-line-numbers-mode 1) (visual-line-mode))))
  :bind
  (("C-c pc" . pbcopy))
  :custom
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (visible-cursor nil)
  (inhibit-startup-screen t)
  (visible-bell nil)
  (ring-bell-function 'ignore)
  (initial-major-mode 'text-mode)
  (initial-scratch-message "")
  (fill-column 70)
  (split-height-threshold nil)
  (split-width-threshold 0)
  (redisplay-dont-pause t)
  (scroll-margin 5)
  (scroll-step 1)
  (scroll-preserve-screen-position 1)
  (tab-width 4)
  (indent-tabs-mode nil)
  (echo-keystrokes 0.01)
  (use-short-answers t)
  (create-lockfiles nil)
  (backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 20)
  (kept-old-versions 5)
  (mode-line-format
   '(" "
     (:eval (buffer-name))
     " | "
     "%l:%c"
     " | "
     "%m"
     " | "
     (:eval (if (buffer-modified-p) "*" "-"))
     " | "
     (:eval (when (buffer-file-name)
              (abbreviate-file-name (buffer-file-name))))))
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (window-divider-mode 1)
  (windmove-default-keybindings)
  (set-face-attribute 'default nil :font "Rec Mono Casual" :height 180)
  (line-number-mode 1)
  (column-number-mode 1)
  (global-auto-revert-mode 1)
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  (defun pbcopy ()
    (interactive)
    (call-process-region (point) (mark) "pbcopy")
    (setq deactivate-mark t)))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package ibuffer
  :straight t
  :bind ("C-x C-b" . ibuffer)
  :config
  (defun my/clear-buffers ()
    (interactive)
    (mapc (lambda (buffer)
            (unless (or (string= (buffer-name buffer) "*scratch*")
                        (string= (buffer-name buffer) "*Ibuffer*"))
              (kill-buffer buffer)))
          (buffer-list))
    (ibuffer-update nil t))
  (define-key ibuffer-mode-map (kbd "C-c k") 'my/clear-buffers))

(use-package catppuccin-theme
  :straight t
  :init
  (setq catppuccin-flavor
        (if (string= (string-trim (shell-command-to-string "dark-mode status")) "on")
            'frappe
          'latte))
  :config
  (load-theme 'catppuccin t)
  (defvar my/red (catppuccin-get-color 'red))
  (defvar my/green (catppuccin-get-color 'green))
  (defvar my/yellow (catppuccin-get-color 'yellow))
  (defvar my/blue (catppuccin-get-color 'blue))
  (defvar my/purple (catppuccin-get-color 'mauve))
  (defvar my/pink (catppuccin-get-color 'pink))
  (defvar my/teal (catppuccin-get-color 'teal))
  (defvar my/background (catppuccin-get-color 'surface0))

  (defvar my/tag-colors
    `(("jobs" . ,my/blue)
      ("money" . ,my/green)
      ("research" . ,my/pink)
      ("plans" . ,my/pink)
      ("general" . ,my/purple)
      ("school" . ,my/blue)
      ("birthdays" . ,my/blue)
      ("csds_343" . ,my/blue)
      ("csds_456" . ,my/yellow)
      ("csds_435" . ,my/purple)
      ("csds_570" . ,my/pink)
      ("econ_341" . ,my/green)
      ("math_324" . ,my/teal)
      ("csds_391" . ,my/pink)
      ("contract" . ,my/pink)))
  )

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 7)
  (vertico-resize nil)
  (vertico-directory-delete-word)
  )

(use-package orderless
  :custom
  (completion-styles '(orderless)))

(use-package org
  :init
  (setq notes-file (shell-command-to-string "$SHELL -c 'echo -n $TODO_FILE'"))
  (setq org-default-notes-file notes-file)
  (setq org-directory (file-name-directory notes-file))
  :hook
  (org-mode . org-indent-mode)
  (org-agenda-mode . (lambda ()
                       (hl-line-mode 1)
                       (setq-local line-spacing 2)
                       (keymap-set org-agenda-mode-map "<remap> <forward-paragraph>" nil)
                       (keymap-set org-agenda-mode-map "<remap> <backward-paragraph>" nil)))
  :bind
  ("C-c c" . org-capture)
  ("C-c d" . (lambda () (interactive) (org-agenda nil "d")))
  ("C-c o" . (lambda ()
               (interactive)
               (let* ((org-files (seq-filter (lambda (file)
                                               (not (string-prefix-p "." file)))
                                             (directory-files org-directory)))
                      (selected-file (completing-read "Search Orgs: " org-files)))
                 (find-file (expand-file-name selected-file org-directory)))))
  :custom
  (org-outline-path-complete-in-steps nil)
  (org-refile-targets '((org-default-notes-file . (:maxlevel . 1))))
  (org-image-actual-width 400)
  (org-hide-emphasis-markers t)
  (org-tags-column 1)
  (org-agenda-tags-column 0)
  (org-fold-show-context-detail t)
  (org-ellipsis "⤵")
  (org-agenda-files (list org-default-notes-file))
  (org-agenda-prefix-format
   '((agenda . " %?-10T %?-12t %s")
     (todo . " %-10T%(my/timestamp-format)")))
  (org-agenda-deadline-leaders (quote ("!D!: " "D %d: " "L %d: ")))
  (org-agenda-remove-tags t)
  (org-todo-keywords
   '("TODO(t)" "DOING(g)" "DAY" "WAITING" "EVENT(e)" "REMINDER(r)" "DONE(d)"))
  (org-agenda-block-separator ?─)
  (org-agenda-custom-commands
   '(("d" "Dashboard"
      (
       (todo "DAY"
             ((org-agenda-overriding-header "")))
       (agenda ""
               ((org-agenda-start-day (org-today))
                (org-agenda-span 1)
                (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                (org-agenda-format-date "%A %-e %B %Y")
                (org-agenda-overriding-header "")))
       (todo "TODO|DOING"
             ((org-agenda-sorting-strategy '(priority-down))
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'regexp "\\[#C\\]" 'timestamp))
              (org-agenda-overriding-header "")))
       (todo "TODO|DOING"
             ((org-agenda-sorting-strategy '(timestamp-up))
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "\\[#C\\]" 'nottimestamp))
              (org-agenda-block-separator nil)
              (org-agenda-overriding-header "")))
       (todo "TODO|DOING"
             ((org-agenda-sorting-strategy '(timestamp-up))
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'notregexp "\\[#C\\]"))
              (org-agenda-overriding-header "")))
       (todo "WAITING"
             ((org-agenda-sorting-strategy '(timestamp-up))
              (org-agenda-overriding-header "")))
       (todo "EVENT"
             ((org-agenda-sorting-strategy '(timestamp-up))
              (org-agenda-overriding-header "")))
       (agenda ""
               ((org-agenda-start-day "+1d")
                (org-agenda-span 10)
                (org-agenda-day-face-function (lambda (date) '(:underline t :inherit org-agenda-date)))
                (org-agenda-overriding-header "")
                ))
       (todo "REMINDER"
             ((org-agenda-sorting-strategy '(timestamp-up))
              (org-agenda-overriding-header "")))
       (todo "DONE"
             ((org-agenda-overriding-header "")))
       )
      ((org-agenda-window-setup 'only-window)
       ))))
  (org-emphasis-alist
   `(("*" (:weight bold :foreground ,my/blue))
     ("/" italic)
     ("_" underline)
     ("=" org-verbatim verbatim)
     ("~" org-code verbatim)
     ("+" (:strike-through t))))
  (org-highlight-latex-and-related '(latex))
  :config
  (setq org-default-priority ?C)
  (with-eval-after-load "org-faces"
    (defun my/create-keyword-face (main-color)
      `((t :weight bold
           :foreground ,main-color)))

    (defface my/org-red (my/create-keyword-face my/red)
      "Face used to display state TODO.")
    (defface my/org-green (my/create-keyword-face my/green)
      "Face used to display state DONE.")
    (defface my/org-yellow (my/create-keyword-face my/yellow)
      "Face used to display state DOING.")
    (defface my/org-purple (my/create-keyword-face my/purple)
      "Face used to display state DOING.")
    (defface my/org-pink (my/create-keyword-face my/pink)
      "Face used to display state EVENT.")
    (defface my/org-blue (my/create-keyword-face my/blue)
      "Face used to display state DAY.")
    (setq org-todo-keyword-faces
          '(("TODO" . my/org-red)
            ("DOING" . my/org-yellow)
            ("DONE" . my/org-green)
            ("EVENT" . my/org-purple)
            ("DAY" . my/org-blue)
            ("WAITING" . my/org-pink)
            ("REMINDER" . my/org-blue)
            ))
    )

  (defun my/timestamp-format ()
    "Custom function to format timestamps for TODO items."
    (let ((timestamp
           (or (org-entry-get (point) "TIMESTAMP")
               (org-entry-get (point) "SCHEDULED")
               (org-entry-get (point) "DEADLINE"))))
      (if timestamp
          (let* ((parsed-time (org-time-string-to-time timestamp))
                 (formatted-date (format-time-string "%d %a %m-%Y" parsed-time))
                 (time-component (format-time-string "%H:%M" parsed-time))
                 (has-time (and (string-match-p "\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)" timestamp)
                                (not (string-match-p "T00:00" timestamp)))))
            (concat (format-time-string "%d %a" parsed-time)
                    (when has-time (concat " " time-component))
                    (concat " " (format-time-string "%m-%Y " parsed-time))))
        "")))

  (set-face-underline 'org-ellipsis nil)
  (defun save-after-capture-refile ()
    (with-current-buffer (marker-buffer org-capture-last-stored-marker)
      (save-buffer)))
  (advice-add 'org-capture-refile :after 'save-after-capture-refile)

  (defvar org-capture-templates
    '(("t"
       "Todo Item"
       entry
       (file org-default-notes-file)
       "* TODO %?\n %i")
      ("e"
       "Event"
       entry
       (file org-default-notes-file)
       "* EVENT %?\n %i")
      ))

  (defun my/org-agenda-custom-color ()
    "Customize the appearance of Org Agenda lines with keywords."
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line-start (point)))
          (when (re-search-forward "^\\s-*\\(\\S-+\\)" (line-end-position) t)
            (let* ((keyword (match-string 1))
                   (color (cdr (assoc keyword my/tag-colors))))
              (when color
                (add-text-properties
                 (match-beginning 1) (match-end 1)
                 `(face (:foreground ,color))))))
          (forward-line 1)))))
  (add-hook 'org-agenda-finalize-hook #'my/org-agenda-custom-color)
  )

(defun org-capture-full ()
  (interactive)
  (org-capture)
  (delete-other-windows)
  (advice-add 'org-refile :after
              (lambda (&rest _)
                (save-some-buffers t)
                (kill-emacs))))

(use-package org-superstar
  :straight t
  :custom
  (org-superstar-item-bullet-alist
   '((?- . ?•)))
  :hook (org-mode . org-superstar-mode))

(use-package avy
  :bind
  ("C-." . 'avy-goto-char))

(use-package nerd-icons
  :straight t)
(use-package nerd-icons-completion
  :straight t
  :config
  (nerd-icons-completion-mode))
(use-package nerd-icons-ibuffer
  :straight t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))
(use-package nerd-icons-dired
  :straight t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package eglot
  :straight t
  :hook
  (typst-ts-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  :custom
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (eglot-report-progress nil)
  (eglot-code-action-indications nil)
  (eglot-autoshutdown t)
  (eglot-extend-to-xref nil)
  (eglot-diagnostic-frontend 'flycheck)
  (eglot-events-buffer-config '(:size 0 :format short))
  (eglot-stay-out-of '(minibuffer-echo-area))
  :config
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(typst-ts-mode . ("tinymist" :initializationOptions nil))))

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode))
(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))
(use-package flycheck-inline
  :straight t
  :after flycheck
  :hook
  (flycheck-mode . flycheck-inline-mode))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-count 4)
  :bind
  (:map corfu-map
        ("RET" . nil))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :straight t
  :init
  (add-hook 'completion-at-point-functions (cape-capf-prefix-length #'cape-dabbrev 4))
  (add-hook 'completion-at-point-functions #'cape-file)
  (defun my/eglot-cape-setup ()
    (setq-local completion-at-point-functions
                (list #'cape-file
                      (cape-capf-prefix-length #'cape-dabbrev 4)
                      #'eglot-completion-at-point)))
  :hook
  (eglot-managed-mode . my/eglot-cape-setup))

(use-package typst-ts-mode
  :straight '(:type git :host codeberg :repo "meow_king/typst-ts-mode"))
(use-package pyvenv
  :bind
  ("C-c p p a" . 'pyvenv-activate)
  ("C-c p p d" . 'pyvenv-deactivate)
  :config
  (pyvenv-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
