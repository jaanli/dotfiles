(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

; list the packages you want
(setq package-list
      '(ein deft helm color-theme-solarized))

; activate all the packages
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; use ido for files
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (deft ein color-theme-solarized)))
 '(python-indent-guess-indent-offset nil)
 '(python-indent-offset 2))

; use dark solarized color scheme
(load-theme 'solarized t)
(setq frame-background-mode 'dark)

; deft customization
(global-set-key (kbd "<f8>") 'deft)
(setq deft-use-filename-as-title t)
(setq deft-use-filter-string-for-filename t)
(setq deft-directory "/Users/jaan/Library/Application\sSupport/Notational\sData")
(setq deft-extension "txt")
; associate files in deft directory with org mode
(add-to-list 'auto-mode-alist '("/Users/jaan/Library/Application\sSupport/Notational\sData/.*[.]txt$" . org-mode))


; disable menubar
(menu-bar-mode -1)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; use ipython with autoreload for python interpreter
; issue with python interpreter - cannot use cd, ls commands?
;; (setq
;;    python-shell-interpreter "ipython"
;;    python-shell-interpreter-args "--profile=dev --simple-prompt"
;; )

; helm configuration
(require 'helm-config)
(helm-mode 1)
; use helm to navigate kill ring
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
; use helm-mode to search for commands
(global-set-key (kbd "M-x") 'helm-M-x)

; scroll to bottom on output
; may need to disable for sane error navigation in future
;; (setq comint-scroll-to-bottom-on-output t)
;; (setq comint-move-point-for-output t)

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; python path for anaconda python
(setenv "PYTHONPATH" (shell-command-to-string "$SHELL --login -c 'echo -n $PYTHONPATH'"))
