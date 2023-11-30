;;; init.el --- Timothy's cool init file
;;; Commentary:
;; Nice customizations for Emacs.

;;; Code:
;; useful for quickly debugging emacs
;; (setq debug-on-error t)

(server-start)

;; things that will be changed often
(defvar my/red "#FF6F61")
(defvar my/light-red "#FFB6B0")
(defvar my/green "#77DD77")
(defvar my/light-green "#B5EAD7")
(defvar my/orange "#FFB347")
(defvar my/light-orange "#FFD180")
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

;; everforest colors maybe use: https://github.com/sainnhe/everforest
; #d3c6aa
; #e67e80
; #e69875
; #dbbc7f
; #a7c080
; #83c092
; #7fbbb3
; #d699b6
; #7a8478
; #859289
; #9da9a0
; #a7c080
; #d3c6aa
; #e67e80

(defvar my/docs-dir "~/Documents/")
(defvar my/school-dir (concat my/docs-dir "school/"))
(defvar my/307-dir (concat my/school-dir "math_307/"))
(defvar my/421-dir (concat my/school-dir "math_421/"))
(defvar my/307-textbook (concat my/307-dir "textbook_needs_errata.pdf"))
(defvar my/apps-dir (concat my/docs-dir "applications/"))
(defvar my/cover-letter-dir (concat my/apps-dir "cover_letter/"))
(defvar my/resume-dir (concat my/apps-dir "resume/"))
(defvar my/window-configs
  (list
    (list "307 Notes"
          (concat my/307-dir "notes.org")
          my/307-textbook)
    (list "307 HW"
          my/307-textbook
          nil)
    (list "421 Notes"
          (concat my/421-dir "notes.org")
          nil)
    (list "cover letter"
          (concat my/cover-letter-dir "cover_letter.tex")
          (concat my/cover-letter-dir "cover_letter.pdf")
          )
    (list "resume"
          (concat my/resume-dir "resume.tex")
          (concat my/resume-dir "resume.pdf")
          )
    ))

;; Basic styling
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 0)
(tooltip-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(windmove-default-keybindings)
(setq inhibit-startup-screen t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "")
(set-face-attribute 'default nil :font "mononoki" :height 190)
(column-number-mode)
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Buffer things
(setq-default tab-width 4
              indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default python-indent-offset 4)
(global-visual-line-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)
(defun add-newline-at-end-if-missing ()
  "Add a newline at the end of the buffer if it's missing."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))))
(add-hook 'before-save-hook 'add-newline-at-end-if-missing)

;; mini buffer
(setq echo-keystrokes 0.01)
(setopt use-short-answers t)

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

(when (string= system-type "darwin")
  (defvar dired-use-ls-dired nil))

;; change backups location
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; env
(setenv "SHELL" (shell-command-to-string "which zsh"))
(setenv "JAVA_HOME" (shell-command-to-string "/usr/libexec/java_home"))

;; global-key-bindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-c p c") 'my/compile)
(global-set-key (kbd "C-c o w") 'my/pick-window-config)
(global-set-key (kbd "C-c o s") 'my/search-school-directory)
(setq compile-command nil)
(defvar tex-compile-commands '(("latexmk -pdf -pvc %f")))
(defun my/compile ()
  "Compile depending on the context: project or LaTeX mode."
  (interactive)
  (if (eq major-mode 'latex-mode)
      (progn
        (delete-other-windows)
        (split-window-horizontally)
        (call-interactively 'tex-compile)
        )
      (if (project-current)
          (call-interactively 'project-compile)
        (call-interactively 'compile))))

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell/alias "batt" "pmset -g batt | awk '/InternalBattery/ {print $3, $4}'")
            (eshell/alias "todo" "gret TODO")
            (eshell/alias "clear" "clear-scrollback")
            (my/eshell-setup)))

(defun my/eshell-setup ()
  (define-key eshell-mode-map "\C-a" 'my/eshell-maybe-bol)
  (set-face-foreground 'eshell-prompt my/green))

(defvar my/eshell-prompt-ending "╰──% ")

(defun my/put-tilde-in-path (path)
  "Shorten PATH by replacing HOME with ~."
  (let ((home (expand-file-name (getenv "HOME"))))
    ;; Remove any trailing slash from the home directory
    (setq home (if (string-suffix-p "/" home)
                   (substring home 0 -1)
                 home))
    (if (or (string-prefix-p home path)
            (string-prefix-p (substring home 1) path))
        (if (string-prefix-p home path)
            (concat "~" (substring path (length home)))
          (concat "~" (substring path (1- (length home)))))
      path)))

(setq eshell-prompt-function
      (lambda ()
        (let* ((pwd (eshell/pwd))
               (home (expand-file-name (getenv "HOME")))
               (colored-pwd (propertize (my/put-tilde-in-path pwd) 'face `(:foreground ,my/purple)))
               (env-name (getenv "VIRTUAL_ENV"))
               (rel-env-path (my/put-tilde-in-path (when env-name
                               (file-relative-name env-name pwd)))))
          (concat "╭─"
                  "[" colored-pwd "]"
                  (if rel-env-path
                      (concat "(" (propertize rel-env-path 'face '(:foreground ,my/light-purple)) ")")
                    "")
                  (my/curr-dir-git-info pwd)
                  "\n" my/eshell-prompt-ending
                  )))
      eshell-prompt-regexp (concat "^" (regexp-quote my/eshell-prompt-ending)))

(defun my/curr-dir-git-info (pwd)
  "Returns current git branch as a string, with different colors based on the status."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let* ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'")))
           (branch (if (> (length git-output) 0)
                       (substring git-output 0 -1)
                     "(no branch)"))
           (status (shell-command-to-string (concat "cd " pwd " && git status --porcelain")))
           (branch-color (if (string-match-p "[^\s]" status) my/red my/green))
           (branch-with-color (propertize branch 'face `(:foreground ,branch-color))))
      (concat "[" branch-with-color "]"))))

(defun my/eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))

(defun my/fit-file (file)
  (when (string-suffix-p ".pdf" file)
    (pdf-view-fit-page-to-window))
  )

(defun my/open-two-files-vertically (a b)
  (delete-other-windows)
  (find-file a)
  (when (not b)
    (my/fit-file a))
  (when b
    (split-window-right)
    (my/fit-file a)
    (other-window 1)
    (find-file b)
    (my/fit-file b)
    (other-window 1)
    ))

(defun my/pick-window-config ()
  (interactive)
  (let* ((chosen-config (completing-read "Pick a configuration: " my/window-configs nil t))
         (selected-config (assoc chosen-config my/window-configs))
         (file-a (if selected-config (car (cdr selected-config))))
         (file-b (if selected-config (car (cdr (cdr selected-config))))))
    (when selected-config
      (my/open-two-files-vertically file-a file-b))))

;; ibuffer
(defun my/clear-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (unless (or (string= (buffer-name buffer) "*scratch*")
                      (string= (buffer-name buffer) "*Ibuffer*"))
            (kill-buffer buffer)))
        (buffer-list))
  (ibuffer-update nil t))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c k") 'my/clear-buffers)))

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("main"  (not (name . "^\\*")))
              ))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

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

(defun my/search-school-directory ()
  "Search for files in school directory using Vertico."
  (interactive)
  (let ((default-directory my/school-dir))
    (call-interactively 'find-file)))

(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

;; packages
(require 'package)

(package-initialize)
;; initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; (package-refresh-contents)

(use-package modus-themes
  :init
  (load-theme 'modus-vivendi-tinted t))

(use-package rainbow-mode
  :hook (emacs-lisp-mode text-mode lisp-mode)
  :custom
  (rainbow-x-colors nil)
  )

;; (use-package evil
;;   :config
;;   (setq evil-emacs-state-cursor `(,my/purple box))
;;   (setq evil-normal-state-cursor `(,my/green box))
;;   (setq evil-visual-state-cursor `(,my/light-blue box))
;;   (setq evil-insert-state-cursor `(,my/light-yellow box))
;;   (setq evil-replace-state-cursor `(,my/orange bar))
;;   (setq evil-operator-state-cursor `(,my/orange hollow))
;;   (evil-mode 1))

;; better editing
(use-package multiple-cursors
  :bind
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this))

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
  )

;; suggestions
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 7)
  (vertico-resize nil)
  (vertico-directory-delete-char)
  (vertico-directory-delete-word)
  )

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :custom
  (completion-styles '(orderless basic)
                     completion-category-defaults nil
                     completion-category-overrides '((file (styles partial-completion)))))

(use-package which-key
  :custom
  (which-key-idle-delay 1.0)
  :config
  (which-key-mode 1))

;; run M-x nerd-icons-install-fonts
(use-package doom-modeline
  :custom
  (doom-modeline-height 20)
  (display-time-format "%H:%M")
  :config
  (doom-modeline-mode)
  (display-time-mode)
  (display-battery-mode)
  )

;; latex and pdf previews
;; PDF preview
;; to install latex:
;; - Install basictex
;; - sudo tlmgr option repository https://mirrors.rit.edu/CTAN/systems/texlive/tlnet/
;; - sudo tlmgr update –self
;; - If needed: sudo tlmgr install <your_package_name>
;; - sudo tlmgr install dvisvgm/dvipng # for math preview
;; - sudo tlmgr install latexmk # for better compilation
;; other dependencies
;; - ghostscript

;; org-mode
(use-package org
  :hook
  (org-mode . org-indent-mode)
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
        (todo . "%-10T%-14(my/timestamp-format) ")
        (tags  . " %i %-12:c")
        (search . " %i %-12:c")))
  (org-agenda-remove-tags t)
  (org-agenda-span 14)
  (org-todo-keywords
      '("TODO(t)" "DOING(g)" "EVENT(e)" "LONG(l)" "DONE(d)" "REMINDER(r)"))
  ;; (org-startup-with-latex-preview t) ;; this is very slow for some reason and renders with white background and foreground
  (org-columns-default-format "%10ALLTAGS %TODO %30ITEM %22SCHEDULED %22DEADLINE %TIMESTAMP")
  (org-agenda-custom-commands
   '(("d" "Dashboard"
      (
       (agenda ""
               ((org-agenda-start-day (org-today))
                (org-agenda-span 1)
                (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                (org-agenda-format-date "%A %-e %B %Y")
                (org-agenda-overriding-header "")
                ))
       (todo "TODO|DOING"
             ((org-agenda-sorting-strategy '(priority-down timestamp-up))
              (org-agenda-overriding-header "")))
       (agenda ""
               ((org-agenda-start-day "+1d")
                (org-agenda-span 10)
                (org-agenda-day-face-function (lambda (date) '(:underline t :inherit org-agenda-date)))
                (org-agenda-overriding-header "")
                ))
       (todo "EVENT"
             ((org-agenda-sorting-strategy '(timestamp-up))
              (org-agenda-overriding-header "")))
       (todo "DONE"
             ((org-agenda-overriding-header "")))
       (todo "LONG"
             ((org-agenda-overriding-header "")))
       (todo "REMINDER"
             ((org-agenda-overriding-header "")))
       )
      ((org-agenda-window-setup 'only-window)
       ))))
  :config
  (with-eval-after-load "org-faces"

    (setq org-todo-keyword-faces
          '(("TODO" . my/org-todo)
            ("DOING" . my/org-doing)
            ("DONE" . my/org-done)
            ("EVENT" . my/org-event)
            ("LONG" . my/org-long)
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
    (defface my/org-long (my/create-keyword-face my/blue my/light-blue)
      "Face used to display state LONG.")
    (defface my/org-reminder (my/create-keyword-face my/turquoise my/light-turquoise)
      "Face used to display state LONG.")
    (set-face-attribute 'org-level-1 nil :inherit 'outline-1 :height 1.3)
    (set-face-attribute 'org-level-2 nil :inherit 'outline-2 :height 1.25)
    (set-face-attribute 'org-level-3 nil :inherit 'outline-3 :height 1.2)
    (set-face-attribute 'org-level-4 nil :inherit 'outline-4 :height 1.15)
    (set-face-attribute 'org-level-5 nil :inherit 'outline-5 :height 1.1)
    (set-face-attribute 'org-level-6 nil :inherit 'outline-6 :height 1.05)
    (set-face-attribute 'org-level-7 nil :inherit 'outline-7 :height 1.0)
    (set-face-attribute 'org-level-8 nil :inherit 'outline-8 :height 1.0)
    (set-face-attribute 'org-tag nil :height 0.6)
    )
  (setq org-emphasis-alist
        `(("*" (:weight bold :foreground ,my/blue))
          ("/" italic)
          ("_" underline)
          ("=" org-verbatim verbatim)
          ("~" org-code verbatim)
          ("+" (:strike-through t))))

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
          (if (string-match "<%%(diary-last-day-of-month date)>" timestamp)
              (let ((date-str (match-string 1 timestamp))
                    (cal-date (org-eval-in-calendar `(diary-last-day-of-month ,date-str))))
                (format-time-string "%Y-%m-%d" cal-date))
            (replace-regexp-in-string "[<>]" "" timestamp))
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
       "* TODO %?\n %i")))

  (defvar my/tag-colors
    `(("jobs" . ,my/blue)
      ("projects" . ,my/purple)
      ("money" . ,my/light-green)
      ("math_307" . ,my/green)
      ("math_421" . ,my/orange)
      ("csds_341" . ,my/purple)
      ("phed_130" . ,my/light-purple)
      ("phed_24b" . ,my/light-purple)
      ("csds_393" . ,my/light-blue)
      ("econ_216" . ,my/brown)
      ("aim4" . ,my/yellow)
      ("rwc" . ,my/light-green)
      ("medical" . ,my/light-red)))

  (defun my/org-agenda-custom-color ()
    "Customize the appearance of Org Agenda lines with keywords."
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (regexp-opt (mapcar #'car my/tag-colors)) nil t)
        (let ((keyword (match-string 0)))
          (add-text-properties
           (match-beginning 0) (match-end 0)
           `(face (:foreground ,(cdr (assoc keyword my/tag-colors)))))))))

  (add-hook 'org-agenda-finalize-hook #'my/org-agenda-custom-color)
  (setq org-default-priority ?C)
  )

;; math preview
(use-package org-fragtog
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-preview-latex-default-process 'dvisvgm)
  (org-preview-latex-image-directory (concat user-emacs-directory "ltximg/"))
  ;; https://dvisvgm.de/FAQ/
  (org-preview-latex-process-alist '((dvisvgm :programs
                                              ("latex" "dvisvgm")
                                              :image-input-type "dvi"
                                              :image-output-type "svg"
                                              :image-size-adjust (1.7 . 1.5)
                                              :latex-compiler
                                              ("latex -interaction nonstopmode -output-directory %o %f")
                                              :image-converter
                                              ("dvisvgm %f --libgs=/opt/homebrew/lib/libgs.dylib --no-fonts --exact-bbox --scale=%S --output=%O"))))
  :config
  (plist-put org-format-latex-options :scale 2.0)
  (plist-put org-format-latex-options :foreground nil)
  (plist-put org-format-latex-options :background nil)
  )

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))


;; coding
;; magit
(use-package magit
  :bind ("C-x g"   . magit-status)
  )

;; lsp
(use-package eglot
  :hook
  ;; (rust-mode . eglot-ensure)
  ;; (elisp-mode . eglot-ensure)
  ;; (c-mode . eglot-ensure)
  ;; (js-mode . eglot-ensure)
  ;; (python-mode . eglot-ensure)
  (eglot . eglot-ensure)
  :custom
  (eglot-ignored-server-capabilities '(:hoverProvider)))

;; syntax reports
(use-package flymake
  :hook
  (emacs-lisp-mode . flymake-mode)
  :bind (("C-c f d"   . flymake-show-buffer-diagnostics)
         ("C-c f D" . flymake-show-project-diagnostics)
         ("C-c f ." . flymake-goto-next-error)
         ("C-c f ," . flymake-goto-prev-error)
         )
  :hook
  (after-save-hook . my/flymake-refresh-errors)
  :custom
  (defun my/flymake-refresh-errors ()
    "Restart \"flymake\" to refresh reporting."
    (when flymake-mode
      (flymake-mode 0)
      (flymake-mode 1))
    )
)

;; completions
(use-package corfu
  :hook
  (rust-mode .
          (lambda () (setq indent-tabs-mode nil)))
  (eshell-mode . (lambda ()
                   (setq-local corfu-auto nil))
               )
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

(use-package yasnippet
  :custom
  (yas-snippet-dir "~/.emacs.d/snippets")
  :config
  (yas-global-mode 1)
  )

(use-package avy
  :bind
  ("C-." . 'avy-goto-char)
  )

(use-package git-modes)

(use-package markdown-mode)

;; python
;; 1. *Command:* /pip3 install python-lsp-server[all]/
;; 2. put the pylsp in path
(use-package python-mode)
(use-package pyvenv
  :bind
  ("C-c p p a" . 'pyvenv-activate)
  ("C-c p p d" . 'pyvenv-deactivate)
  :after python-mode
  :config
  (pyvenv-mode 1))

;; rust
;; - /curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh/ this placed file in ~/.cargo/
;; - To uninstall run: /rustup self uninstall/ from https://rust-lang.github.io/rustup/installation/index.html
;; - for lsp: /rustup component add rust-analyzer/
;; - locate its location with: rustup which rust-analyzer
;; - add that path to /$PATH/

(use-package rust-mode
  :hook
  (rust-mode .
             (lambda () (setq indent-tabs-mode nil)))
  :config
  :init
  (defvar rust-format-on-save t))

;; Java
;; After brew install openjdk do the command shown by brew info openJDK
;; sudo ln -sfn /opt/homebrew/opt/openjdk@11/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk-11.jdk

;; JavaScript
;; - npm install -g typescript typescript-language-server
;; - put the typescript-language-server in /usr/local/bin
;; - to use create jsconfig.json or tsconfig.json
(use-package prettier-js
  :hook
  (js-mode . prettier-js-mode)
  )

;; go lang
;; go install golang.org/x/tools/gopls@latest
(use-package go-mode
  :hook
  (go-mode . eglot-format-buffer-on-save)
  )

(use-package glsl-mode)

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
   '("b40f11c174e7e475508f1e2c1cfca354d37212494c143a494f27239c7d71a294" "e5ce8ca9850b68052affa5b3cc69bb97abf7a8a76e1088ea3907fdabeaeb5036" default))
 '(package-selected-packages
   '(avy yasnippet which-key vertico rust-mode rainbow-mode pyvenv python-mode prettier-js pdf-tools org-fragtog orderless nerd-icons-ibuffer nerd-icons-dired nerd-icons-completion multiple-cursors markdown-mode magit go-mode glsl-mode evil doom-modeline corfu)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
