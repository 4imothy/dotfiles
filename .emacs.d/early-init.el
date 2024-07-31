;;; init.el --- Timothy's cool init file
;;; Commentary:
;; Nice customizations for Emacs.
;;; Code:

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))
(setq vc-follow-symlinks t)

;;; early-init.el ends here
