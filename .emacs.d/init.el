;;; init.el --- Timothy's cool init file
;;; Commentary:
;; Nice customizations for Emacs.

;;; Code:
;; useful for quickly debugging emacs
;; (setq debug-on-error t)

(server-start)

;; make fullscreen and edit menu items
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 5)
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

;; line wrapping
(global-visual-line-mode)

;; make command prefixes show fast
(setq echo-keystrokes 0.01)

;; remove whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)

(defun add-newline-at-end-if-missing ()
  "Add a newline at the end of the buffer if it's missing."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (unless (bolp) ; Check if the point is already at the beginning of a line
      (insert "\n"))))

(add-hook 'before-save-hook 'add-newline-at-end-if-missing)

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
(global-set-key (kbd "C-x p c") 'my/compile)
(global-set-key (kbd "C-c e") 'eshell)

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell/alias "batt" "pmset -g batt | awk '/InternalBattery/ {print $3, $4}'")
            (eshell/alias "todo" "gret TODO")))

(defvar my/eshell-prompt-ending "╰──% ")
(setq eshell-prompt-function
      (lambda ()
        (let* ((pwd (eshell/pwd))
               (home (expand-file-name (getenv "HOME")))
               (abbreviated-pwd (if (string-prefix-p home pwd)
                                    (concat "~" (substring pwd (length home)))
                                  pwd))
               (colored-pwd (propertize abbreviated-pwd 'face `(:foreground "#e9e2cb"))))
          (concat "╭─[" colored-pwd "]" "\n" my/eshell-prompt-ending )))
      eshell-prompt-regexp (concat "^" (regexp-quote my/eshell-prompt-ending)))

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

(defun my-eshell-setup ()
  (define-key eshell-mode-map "\C-a" 'my/eshell-maybe-bol)
  (setq-local face-remapping-alist '((eshell-prompt (:foreground "#40E0D0")))))

(add-hook 'eshell-mode-hook 'my-eshell-setup)

(setenv "SHELL" (shell-command-to-string "which zsh"))
(defun my/eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))

;; ibuffer
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("main"  (not (name . "^\\*")))
              ))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; isearch, from: https://stackoverflow.com/a/36707038/588759
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

;; M-e to move the point to the search string
(defadvice isearch-search (after isearch-no-fail activate)
  "Ensure isearch continues in the same direction if no match is found."
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

(defvar school-directory "~/Documents/School")
(defun my/search-school-directory ()
  "Search for things in the school-directory."
  (interactive)
  (if (file-directory-p school-directory)
      (counsel-find-file school-directory)
    (message "School directory not found.")))

(global-set-key (kbd "C-c o s") 'my/search-school-directory)

;; theme
(load-theme 'modus-vivendi)

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
  ;; Tidy shadowed file names
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
  (org-refile-targets '((org-default-notes-file . (:maxlevel . 1))
                        (org-default-notes-file . (:maxlevel . 2))))
  (org-image-actual-width 400)
  (org-hide-emphasis-markers t)
  (org-tags-column 1)
  (org-agenda-tags-column 0)
  (org-fold-show-context-detail t)
  (org-ellipsis "⤵")
  (org-agenda-files (list org-default-notes-file))
  (org-todo-keywords
   (quote ((sequence "TODO(t)" "|" "DOING(g)" "|" "DONE(d)" "|" "EVENT(e)" ))))
  (org-todo-keyword-faces
      '(("TODO" . (:foreground "red" :weight bold))
        ("DOING" . (:foreground "orange" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("EVENT" . (:foreground "purple" :weight bold))))
  (org-agenda-prefix-format
      '((agenda . " %?-10T %?-12t %s")
        (todo . " %-10T %-14(my/timestamp-format) ")
        (tags  . " %i %-12:c")
        (search . " %i %-12:c")))
  (org-agenda-remove-tags t)
  (org-agenda-span 14)
  ;; (org-startup-with-latex-preview t) ;; this is very slow for some reason and renders with white background and foreground
  (org-columns-default-format "%10ALLTAGS %TODO %30ITEM %22SCHEDULED %22DEADLINE %TIMESTAMP")
  (org-agenda-custom-commands
   '(("d" "Dashboard"
      (
       (tags "+day_plan"
                  ((org-agenda-overriding-header "")))
       (agenda ""
               ((org-agenda-start-day (org-today))
                (org-agenda-span 1)
                (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                (org-agenda-format-date "%A %-e %B %Y")
                (org-agenda-overriding-header "")
                ))
       (todo "TODO|DOING"
             ((org-agenda-sorting-strategy '(timestamp-up))
              (org-agenda-overriding-header "currents")))
       (todo "EVENT"
             ((org-agenda-sorting-strategy '(timestamp-up))
              (org-agenda-overriding-header "events")))
       (agenda ""
               ((org-agenda-start-day "+1d")
                (org-agenda-span 10)
                (org-agenda-day-face-function (lambda (date) '(:underline t :inherit org-agenda-date)))
                (org-agenda-overriding-header "ten days out")
                ))
       (todo "DONE"
             ((org-agenda-overriding-header "")))
       )
      ((org-agenda-window-setup 'only-window)
       ))))
  :config
  (defun my/timestamp-format ()
    "Custom function to format timestamps for TODO items."
    (let ((timestamp
           (or (org-entry-get (point) "TIMESTAMP")
               (org-entry-get (point) "SCHEDULED")
               (org-entry-get (point) "DEADLINE"))))
      (if timestamp
          (replace-regexp-in-string "[<>]" "" timestamp)
        "")))
  (set-face-underline 'org-ellipsis nil)
  (defun save-after-capture-refile ()
    (with-current-buffer (marker-buffer org-capture-last-stored-marker)
      (save-buffer)))
  (advice-add 'org-capture-refile :after 'save-after-capture-refile)

  (defun my/org-file-search ()
    "Search for Org files using Ivy."
    (interactive)
    (let* (
           (org-files (seq-filter (lambda (file) (string-suffix-p ".org" file)) (directory-files org-directory))))
      (ivy-read "Search Orgs: " org-files
                :action (lambda (file)
                          (find-file (expand-file-name file org-directory))))))

  (defun my/dashboard ()
    "Launch the Org Agenda Dashboard custom command."
    (interactive)
    (org-agenda nil "d"))
  ;; (add-hook 'org-after-todo-statistics-hook #'my/org-summary-todo-cookie)
  ;; (add-hook 'org-checkbox-statistics-hook #'my/org-summary-checkbox-cookie)

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
       "Todo Item"
       entry
       (file org-default-notes-file)
       "* TODO %?\n %i")))

  (defvar keyword-colors
    '(("jobs" . "#A3D4E4")
      ("math_307" . "#A5D6A7")
      ("math_421" . "#FFCC80")
      ("csds_341" . "#D8B4E2")
      ("phed_130" . "#D8B4E2")
      ("csds_393" . "#EF9A9A")
      ("econ_216" . "#81C7D4")
      ("aim4" . "#FFF59D")
      ("rwc" . "#B0BEC5")
      ("medical" . "#D8B4E2")))

  (defun my-org-agenda-custom-color ()
    "Customize the appearance of Org Agenda lines with keywords."
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (regexp-opt (mapcar #'car keyword-colors)) nil t)
        (let ((keyword (match-string 0)))
          (add-text-properties
           (match-beginning 0) (match-end 0)
           `(face (:foreground ,(cdr (assoc keyword keyword-colors)))))))))

  (add-hook 'org-agenda-finalize-hook #'my-org-agenda-custom-color)
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

;; coding
;; magit
(use-package magit
  :bind ("C-x g"   . magit-status)
  )

;; compiling
(setq compile-command nil)
(setq tex-compile-commands '(("latexmk -pdf -pvc %f")))
(defun my/compile ()
  "Compile depending on the context: project or LaTeX mode."
  (interactive)
  (if (project-current)
      (project-compile)
    (if (eq major-mode 'latex-mode)
        (call-interactively 'tex-compile)
      (call-interactively 'compile))))

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
         ("C-c f D" . flymake-show-project-diagnostics))
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
  (corfu-preview-current 'insert)
  (corfu-count 4)
  :bind
  (:map corfu-map
        ("RET" . nil))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

;; (use-package treemacs
;;   :bind
;;   ("C-c t" . treemacs)
;;   :custom
;;   (treemacs-persist-file (concat user-emacs-directory "treemacs-persist.org"))
;;   (treemacs-width 25)
;;   (treemacs-filewatch-mode t)
;;   (treemacs-follow-mode t)
;;   )

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

;; treemacs default icons are pixelly
(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package markdown-mode)

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
(setenv "JAVA_HOME" (shell-command-to-string "/usr/libexec/java_home"))

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
(use-package go-mode)

(use-package glsl-mode)

(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-mode-hook #'eglot-format-buffer-on-save)


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
 '(org-tag ((t (:weight bold :height 0.6)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(glsl-mode which-key rust-mode pyvenv python-mode prettier-js org-fragtog multiple-cursors markdown-mode magit go-mode doom-modeline counsel corfu)))
