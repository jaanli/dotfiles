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
      '(ein deft xclip helm auctex-latexmk color-theme-solarized))

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
 '(global-visual-line-mode t)
 '(package-selected-packages
   (quote
    (auctex-latexmk xclip deft ein color-theme-solarized)))
 '(python-indent-guess-indent-offset nil)
 '(python-indent-offset 2)
 '(send-mail-function (quote mailclient-send-it)))

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

; allow copy-pasting to system clipboard
(xclip-mode 1)

; line wrapping
(setq global-visual-line-mode 1)

; helm configuration
(require 'helm-config)
(helm-mode 1)
; use helm to navigate kill ring
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
; use helm-mode to search for commands
(global-set-key (kbd "M-x") 'helm-M-x)

;; ; scroll to bottom on output
;; ; may need to disable for sane error navigation in future
;; (setq comint-scroll-to-bottom-on-output t)
;; (setq comint-move-point-for-output t)

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

; use ipython with autoreload for python interpreter
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "--simple-prompt --classic --nosep"
 ; use custom profile for autoreload magic
 ;python-shell-interpreter-args "--profile=dev --simple-prompt --classic --nosep"
)

;; from http://www.wangzerui.com/2017/02/20/setting-up-a-nice-environment-for-latex-on-macos/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting up latex mode
;; Forward/inverse search with evince using D-bus.
;; Installation:
;; M-x package-install RET auctex RET
;; Tells emacs where to find LaTeX.
(let ((my-path (expand-file-name "/usr/local/bin:/usr/local/texlive/2018/bin/x86_64-darwin")))
(setenv "PATH" (concat my-path ":" (getenv "PATH")))
(add-to-list 'exec-path my-path)) 

;; Auctex Settings
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq Tex-PDF-mode t)

;; https://gist.github.com/stefano-meschiari/9217695
;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook
(lambda ()
  (push
   '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
     :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background  
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

(custom-set-variables
     '(TeX-source-correlate-method 'synctex)
     '(TeX-source-correlate-mode t)
     '(TeX-source-correlate-start-server t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
