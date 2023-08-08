;;; init.el --- Timothy's cool init file
;;; Commentary:
;; Nice customizations for Emacs.
;;; Code:

;; avoid flashing of default white theme
(set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")
(set-face-attribute 'mode-line nil :background "#000000" :foreground "#ffffff" :box 'unspecified)

(defvar my/gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold my/gc-cons-threshold)))
(setq vc-follow-symlinks t)

;;; early-init.el ends here
