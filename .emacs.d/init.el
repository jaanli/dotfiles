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

;; needed for company-anaconda
(require 'rx)

; list the packages you want
(setq package-list
      '(ein deft xclip helm company company-anaconda anaconda-mode auctex-latexmk
	    yasnippet color-theme-solarized))

; activate all the packages
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; for normal desktop command+tab behavior
(menu-bar-mode t)

; use ido for files
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-engine (quote xetex))
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(global-visual-line-mode t)
 '(package-selected-packages
   (quote
    (yasnippet company helm anaconda-mode auctex-latexmk xclip deft ein color-theme-solarized)))
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

;; python settings
;; use ipython with autoreload for python interpreter
(require 'python)
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "--simple-prompt --classic --nosep"
 ; use custom profile for autoreload magic
 ;python-shell-interpreter-args "--profile=dev --simple-prompt --classic --nosep"
)

;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
      ;; python-shell-prompt-detect-failure-warning nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "jupyter")

;; set python to utf-8
(setenv "PYTHONIOENCODING" "utf-8")
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")

;(add-hook 'python-mode-hook 'jedi:setup)
;(setq jedi:complete-on-dot t)
;; autocomplete
(add-hook 'python-mode-hook 'anaconda-mode)

;; options for company autocomplete dropdown package
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; use eldoc for printing function arguments in status bar
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; set line wrapping column default
(setq-default fill-column 80)

;; sentences end with single space
(setq sentence-end-double-space nil)

;; autosave all files
(setq autosave-visited-mode 1)

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
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(setq reftex-plug-into-AUCTeX t)
(setq Tex-PDF-mode t)

;; Reftex and Cleveref
(eval-after-load
    "latex"
  '(TeX-add-style-hook
    "cleveref"
    (lambda ()
      (if (boundp 'reftex-ref-style-alist)
      (add-to-list
       'reftex-ref-style-alist
       '("Cleveref" "cleveref"
         (("\\cref" ?c) ("\\Cref" ?C) ("\\cpageref" ?d) ("\\Cpageref" ?D)))))
      (reftex-ref-style-activate "Cleveref")
      (TeX-add-symbols
       '("cref" TeX-arg-ref)
       '("Cref" TeX-arg-ref)
       '("cpageref" TeX-arg-ref)
       '("Cpageref" TeX-arg-ref)))))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (turn-on-reftex)
            (define-key LaTeX-mode-map (kbd "C-c l") 'reftex-cleveref-Cref)
            (visual-line-mode t)
	    (define-key LaTeX-mode-map (kbd "C-c [") 'reftex-citep)
            t)
	  )

;; Change default equation behavior of reftex
(setq reftex-label-alist '(AMSTeX))

;; https://gist.github.com/stefano-meschiari/9217695
;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook
(lambda ()
  (push
;   '("latexmk" "latexmk -pdfxe -pdfxelatex=\"xelatex --shell-escape -no-pdf -interaction=nonstopmode -synctex=1 %O %S\" %s" TeX-run-TeX nil t
   '("latexmk" "latexmk -xelatex %s" TeX-run-TeX nil t
   :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; Latex
;; Use Skim on macOS to utilize synctex.
;; Confer https://mssun.me/blog/spacemacs-and-latex.html
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)
(setq TeX-source-correlate-method 'synctex)
;; AucTex recognizes some standard viewers, but the default view command
;; does not appear to sync.
(setq TeX-view-program-list
      '(("Okular" "okular --unique %o#src:%n`pwd`/./%b")
        ("Skim" "displayline -b -g %n %o %b")
        ("Zathura"
         ("zathura %o"
          (mode-io-correlate
           " --synctex-forward %n:0:%b -x \"emacsclient +%{line} %{input}\"")))))
(setq TeX-view-program-selection '((output-pdf "Skim")))

;; start emacs in server mode so that skim can talk to it
(server-start)

;; fix non-ASCII input in GUI mode
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
;;(setq mac-option-modifier nil)

; Set PATH, because we don't load .bashrc
; function from https://gist.github.com/jakemcc/3887459
(defun set-exec-path-from-shell-PATH ()
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo -n $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(if window-system (set-exec-path-from-shell-PATH))

;; yasnippet config
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; global font size
(set-face-attribute 'default nil :height 120)

;; recompile shortcut for quick debugging
(global-set-key (kbd "<f5>") 'recompile)

;; use xelatex
;;(setq TeX-engine 'xetex)

;; save open buffers and restore when emacs is restarted
(desktop-save-mode 1)
