;;; init.el --- Timothy's cool init file
;;; Commentary:
;; Nice customizations for Emacs.

;;; Code:
;; useful for quickly debugging emacs
;; (setq debug-on-error t)

(defvar my/red "#FF6F61")
(defvar my/light-red "#FFB6B0")
(defvar my/green "#77DD77")
(defvar my/light-green "#B5EAD7")
(defvar my/orange "#FF8C00")
(defvar my/light-orange "#FFDAB9")
(defvar my/purple "#C882C8")
(defvar my/light-purple "#E8C9E8")
(defvar my/blue "#8AB6D6")
(defvar my/light-blue "#BFD3E6")
(defvar my/brown "#D2B48C")
(defvar my/light-brown "#F4E4D3")
(defvar my/yellow "#FFF081")
(defvar my/light-yellow "#FFFAE6")
(defvar my/turquoise "#71C7D7")
(defvar my/light-turquoise "#AEDFFA")
(defvar my/gray "#A0A0A0")
(defvar my/light-gray "#DDDDDD")

;; Basic styling
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
(menu-bar-mode -1)
;; (tool-bar-mode -1)
(tooltip-mode -1)
(blink-cursor-mode 0)
(windmove-default-keybindings)
(setq visible-cursor nil)
(setq inhibit-startup-screen t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "")
(set-face-attribute 'default nil :font "RecMonoCasual Nerd Font" :height 190)
(setq fill-column 70)

(setq-default mode-line-format
  (list
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
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(setq redisplay-dont-pause t
  scroll-margin 5
  scroll-step 1
  scroll-preserve-screen-position 1)

;; Buffer things
(setq-default tab-width 4
              indent-tabs-mode nil)
(global-visual-line-mode)

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
;; initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
;; (package-refresh-contents)

(use-package server
             :ensure t
             :config
             (unless (server-running-p)
               (server-start)))

(use-package catppuccin-theme
             :ensure t
             :init
             (setq catppuccin-flavor 'frappe)
             :config
             (load-theme 'catppuccin t))

(use-package org
             :hook
             (org-mode . org-indent-mode)
             (org-mode . auto-fill-mode)
             (org-agenda-mode . (lambda () (hl-line-mode 1)))
             ;; fix for the org-startup-with-latex-preview being slow
             ;; (org-mode . (lambda () (mark-whole-buffer) (org-latex-preview) (deactivate-mark)))
             :bind
             ("C-c c" . org-capture)
             ("C-c d" . my/dashboard)
             ("C-c o o" . my/org-file-search)
             :custom
             (org-directory "~/Documents/org")
             (org-default-notes-file (concat org-directory "/tasks.org"))
             (org-outline-path-complete-in-steps nil)
             (org-refile-targets '((org-default-notes-file . (:maxlevel . 1))
                                   (org-default-notes-file . (:maxlevel . 2))))
             (org-image-actual-width 400)
             (org-hide-emphasis-markers t)
             (org-tags-column 1)
             (org-agenda-tags-column 0)
             (org-fold-show-context-detail t)
             (org-ellipsis "⤵")
             (org-agenda-files (list org-default-notes-file))
             (org-agenda-prefix-format
               '((agenda . " %?-10T %?-12t %s")
                 (todo . " %-10T%(my/timestamp-format)")
                 (tags  . " %i %-12:c")
                 (search . " %i %-12:c")))
             (org-agenda-remove-tags t)
             (org-todo-keywords
               '("TODO(t)" "DOING(g)" "DAY" "WAITING" "EVENT(e)" "REMINDER(r)" "DONE(d)"))
             (org-agenda-block-separator ?─)
             (defun my/org-agenda-sort-non-timed-before-timed (a b)
               "Sort agenda items: non-timed before timed."
               (let ((time-a (get-text-property 1 'time-of-day a))
                     (time-b (get-text-property 1 'time-of-day b)))
                 (cond
                   ((and time-a time-b) (< time-a time-b))
                   (time-a nil)
                   (time-b t)
                   (t nil))))
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
                          (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottimestamp))
                          (org-agenda-block-separator nil)
                          (org-agenda-overriding-header "")))
                   (todo "TODO|DOING"
                         ((org-agenda-sorting-strategy '(priority-down))
                          (org-agenda-skip-function
                            '(org-agenda-skip-entry-if 'notregexp "\\[#C\\]"))
                          (org-agenda-overriding-header "")
                          (org-agenda-block-separator nil)))
                   (todo "WAITING"
                         ((org-agenda-sorting-strategy '(priority-down))
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
                 ("$" (:foreground ,my/purple))
                 ("_" underline)
                 ("=" org-verbatim verbatim)
                 ("~" org-code verbatim)
                 ("+" (:strike-through t))))
             :config
             (setq org-default-priority ?C)
             (defun my-org-agenda-skip-timestamp ()
               (let ((timestamp (org-get-scheduled-time (point))))
                 (if timestamp
                   (progn
                     (org-agenda-skip-entry-if 'timestamp)
                     (point-max))
                   (outline-next-heading))))

             (with-eval-after-load "org-faces"
                                   (setq org-todo-keyword-faces
                                         '(("TODO" . my/org-todo)
                                           ("DOING" . my/org-doing)
                                           ("DONE" . my/org-done)
                                           ("EVENT" . my/org-event)
                                           ("LONG" . my/org-long)
                                           ("DAY" . my/org-day)
                                           ("WAITING" . my/org-event)
                                           ("REMINDER" . my/org-reminder)
                                           ))

                                   (defun my/create-keyword-face (main-color background-color)
                                     `((t :weight bold
                                          :box (:line-width 2 :color ,main-color)
                                          :foreground ,main-color
                                          :background ,background-color)))

                                   (defface my/org-todo (my/create-keyword-face my/red my/light-red)
                                            "Face used to display state TODO.")
                                   (defface my/org-done (my/create-keyword-face my/green my/light-green)
                                            "Face used to display state DONE.")
                                   (defface my/org-doing (my/create-keyword-face my/orange my/light-orange)
                                            "Face used to display state DOING.")
                                   (defface my/org-event (my/create-keyword-face my/purple my/light-purple)
                                            "Face used to display state EVENT.")
                                   (defface my/org-day (my/create-keyword-face my/blue my/light-blue)
                                            "Face used to display state DAY.")
                                   (defface my/org-long (my/create-keyword-face my/blue my/light-blue)
                                            "Face used to display state LONG.")
                                   (defface my/org-reminder (my/create-keyword-face my/turquoise my/light-turquoise)
                                            "Face used to display state LONG.")
                                   )
             (defun diary-last-day-of-month (date)
               "Return `t` if DATE is the last day of the month."
               (let* ((day (calendar-extract-day date))
                      (month (calendar-extract-month date))
                      (year (calendar-extract-year date))
                      (last-day-of-month
                        (calendar-last-day-of-month month year)))
                 (= day last-day-of-month)))

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

             (defun my/org-file-search ()
               "Search for Org files using Vertico."
               (interactive)
               (let* ((org-files (seq-filter (lambda (file) (string-suffix-p ".org" file))
                                             (directory-files org-directory)))
                      (selected-file (completing-read "Search Orgs: " org-files)))
                 (find-file (expand-file-name selected-file org-directory))))

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
                  "Todo Item"
                  entry
                  (file org-default-notes-file)
                  "* TODO %?\n %i")
                 ("e"
                  "Event"
                  entry
                  (file org-default-notes-file)
                  "* EVENT %?\n %i")
                 ("a"
                  "Day Task"
                  entry
                  (file org-default-notes-file)
                  "* DAY %?\n %i")
                 ))

             (defvar my/tag-colors
               `(("jobs" . ,my/blue)
                 ("gradedu" . ,my/light-blue)
                 ("projects" . ,my/purple)
                 ("money" . ,my/light-green)
                 ("research" . ,my/orange)
                 ("general" . ,my/light-purple)
                 ("school" . ,my/green)
                 ("PHED_12B" . ,my/blue)
                 ("CSDS_451" . ,my/blue)
                 ("CSDS_465" . ,my/orange)
                 ("MATH_394" . ,my/purple)
                 ("MATH_330" . ,my/light-purple)
                 ("CSDS_395" . ,my/light-green)
                 ("MATH_431" . ,my/brown)))

             (defun my/org-agenda-custom-color ()
               "Customize the appearance of Org Agenda lines with keywords."
               (save-excursion
                 (goto-char (point-min))
                 (while (re-search-forward (regexp-opt (mapcar #'car my/tag-colors)) nil t)
                        (let* ((keyword (match-string 0))
                               (color (cdr (assoc keyword my/tag-colors))))
                          (when color  ;; Only apply if color is not nil
                            (add-text-properties
                              (match-beginning 0) (match-end 0)
                              `(face (:foreground ,color))))))))

             (add-hook 'org-agenda-finalize-hook #'my/org-agenda-custom-color)
             )

(defun org-capture-full ()
  (interactive)
  (org-capture)
  (delete-other-windows))

(use-package nerd-icons)

(use-package nerd-icons-completion
             :config
             (nerd-icons-completion-mode))

(use-package nerd-icons-ibuffer
             :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-dired
             :hook
             (dired-mode . nerd-icons-dired-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(nerd-icons-dired nerd-icons-ibuffer nerd-icons-completion nerd-icons which-key orderless vertico rainbow-mode catppuccin-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
