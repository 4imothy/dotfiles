;;; init.el --- Timothy's cool init file
;;; Commentary:
;; Nice customizations for Emacs.

;;; Code:
;; useful for quickly debugging emacs
;; (setq debug-on-error t)

;; Basic styling
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(window-divider-mode 1)
(windmove-default-keybindings)
(setq visible-cursor nil)
(setq inhibit-startup-screen t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "")
(set-face-attribute 'default nil :font "Rec Mono Casual" :height 180)
(setq fill-column 70)
(setq split-height-threshold nil)
(setq split-width-threshold 0)


(setq-default mode-line-format
              (list
               " "
               '(:eval (buffer-name))
               " | "
               "%l:%c"
               " | "
               "%m"
               " | "
               '(:eval
                 (if (buffer-modified-p)
                     "*"
                   "-"))
               " | "
               '(:eval (when (buffer-file-name)
                         (abbreviate-file-name (buffer-file-name)))))
              )

;; Enable line and column numbers in the mode line
(setq line-number-mode t)
(setq column-number-mode t)
(dolist (mode '(text-mode-hook
                prog-mode-hook
                org-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda ()
                   (display-line-numbers-mode 1)
                   (visual-line-mode))))

(setq redisplay-dont-pause t
      scroll-margin 5
      scroll-step 1
      scroll-preserve-screen-position 1)

;; Buffer things
(setq-default tab-width 4
              indent-tabs-mode nil)

(global-auto-revert-mode 1)

(defun add-newline-at-end-if-missing ()
  "Add a newline at the end of the buffer if it's missing."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))))
(add-hook 'before-save-hook 'add-newline-at-end-if-missing)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; mini buffer
(setq echo-keystrokes 0.01)
(setopt use-short-answers t)

(when (string= system-type "darwin")
  (defvar dired-use-ls-dired nil))

(defun pbcopy ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

(global-set-key (kbd "C-c pc") 'pbcopy)
;; change backups location
(setq create-lockfiles nil)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5
      )

;; isearch, from: https://stackoverflow.com/a/36707038/588759
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)
(defadvice isearch-search (after isearch-no-fail activate)
  "Ensure isearch continues in the same direction if no match is found."
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

;; packages
(require 'package)
(require 'use-package)
(setq use-package-always-ensure t)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
;; (package-refresh-contents)

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package ibuffer
  :ensure t
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
  :ensure t
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

(setq notes-file (shell-command-to-string "$SHELL -c 'echo -n $TODO_FILE'"))
(use-package org
  :init
  (setq org-default-notes-file notes-file)
  (setq org-directory (file-name-directory notes-file))
  :hook
  (org-mode . org-indent-mode)
  (org-agenda-mode . (lambda ()
                       (hl-line-mode 1)
                       (setq-local line-spacing 2)
                       (keymap-set org-agenda-mode-map "<remap> <forward-paragraph>" nil)
                       (keymap-set org-agenda-mode-map "<remap> <backward-paragraph>" nil)))

  ;; fix for the org-startup-with-latex-preview being slow
  ;; (org-mode . (lambda () (mark-whole-buffer) (org-latex-preview) (deactivate-mark)))
  :bind
  ("C-c c" . org-capture)
  ("C-c d" . my/dashboard)
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

  (defun my/dashboard ()
    "Launch the Org Agenda Dashboard custom command."
    (interactive)
    (org-agenda nil "d"))
  ;; (add-hook 'org-after-todo-statistics-hook #'my/org-summary-todo-cookie)
  ;; (add-hook 'org-checkbox-statistics-hook #'my/org-summary-checkbox-cookie)

  ;; from this question: https://emacs.stackexchange.com/questions/7375/can-i-format-cells-in-an-org-mode-table-differently-depending-on-a-formula
  ;; and this person: https://emacs.stackexchange.com/users/15307/erki-der-loony
  (defface positive-face
    `((t :foreground ,my/green))
    "Indicates something positive.")

  (defface negative-face
    `((t :foreground ,my/red))
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
  ;; (font-lock-add-keywords 'org-mode
  ;;                         '(("^ *\\([-]\\) "
  ;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

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
  :ensure t
  :custom
  (org-superstar-item-bullet-alist
   '((?- . ?•)))
  :hook (org-mode . org-superstar-mode))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<wheel-up>") 'scroll-down-line)
  (global-set-key (kbd "<wheel-down>") 'scroll-up-line)
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c46651ab216eb31e699be1bd5e6df8229b08005b534194c1ea92519b09661d71"
     default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
