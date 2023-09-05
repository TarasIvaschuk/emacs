

;; -------- custom variables -----------

;; You will most likely need to adjust this font size for your system!
(defvar my/default-font-size 130)
(defvar my/default-variable-font-size 130)
;; Make frame transparency overridable
(defvar my/frame-transparency '(100 . 100))

;;------------------- package management setup ----------------
;; set Melpa

(require 'package)
(setq package-archives '(("melpa". "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("elpa" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package if it is not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ------------------ clean folders ------------------------

;;  to hold files used by packages
(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))

;; no littering emacs folder
(use-package no-littering)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; backup directory
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

;;------------------ basic UI configuration --------------

;;to stop startup message
(setq inhibit-startup-message t)

(set-fringe-mode 10)        ; Give some breathing room

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha my/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,my/frame-transparency))

;; maximized mode
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; set default font
;(set-face-attribute 'default nil  :font  "Menlo" :height my/default-font-size)
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Set the fixed pitch face
;(set-face-attribute 'fixed-pitch nil :font "Menlo" :height my/default-font-size)

;; Set the variable pitch face
;(set-face-attribute 'variable-pitch nil :font "Menlo" :height my/default-variable-font-size :weight 'regular)


(defun my/set-font-faces ()
  (message "Setting faces!")
  (set-face-attribute 'default nil :font "Menlo" :height my/default-font-size)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "Menlo" :height my/default-font-size)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "Menlo" :height my/default-variable-font-size :weight 'regular))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                ;; (setq doom-modeline-icon t)
                (with-selected-frame frame
                  (my/set-font-faces))))
    (my/set-font-faces))



;; maximize window on startup(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))
;;(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;;(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;;(electric-pair-mode 1)			
;; make electric-pair-mode work on more brackets
;; (setq electric-pair-pairs
;;       '(
;;         (?\" . ?\")
;;         (?\{ . ?\})
;;         (?\' . ?\')))
(global-display-line-numbers-mode t)
(column-number-mode)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;;enable clipboard
(setq select-enable-clipboard t)


;; ----------------------------  UI Configuration --------------------
;; custom load path for themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; load custom theme
;;(load-theme 'dracula t)

;; command log to buffer
(use-package command-log-mode
  :commands comand-log-mode)

;; IVY
(use-package ivy
  :diminish
  :bind (("C-s" . swiper-all)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
	 ("C-s" . ivy-next-line)
         ("C-k" . ivy-previous-line)
	 ("C-r" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
	 ("C-r" . ivy-previous-line)
	 ("C-s" . ivy-next-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
	 ("C-r" . ivy-previous-line)
	 ("C-s" . ivy-next-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))


(use-package swiper
  :after ivy
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)
   ("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)))

;; to navigate in a buffer by letters

(use-package avy
     :bind
     (("M-s" . avy-goto-char)))


;; colorize the brackets

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 3))


;; tweak the counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; c comments //
(add-hook 'c-mode-hook (lambda () (setq comment-start "//"
                                        comment-end   "")))

;; to use shift+ space as prefix to global
;; keybindings  
(use-package general
  :config
  (general-create-definer  my-leader-def
    :prefix "C-C C-SPC")

  (my-leader-def
     "t" '(counsel-load-theme :which-key "choose a theme")))


;; rememder the place in a file
(save-place-mode 1)


;; remember the history in the mini buffer(alt + p , alt + n)
(setq history-length 10)
(savehist-mode 1)

;; revert buffers when underlying file has changed
(global-auto-revert-mode 1)

;; revert dired and other buffers
(setq global-auto-revert-non-file-buffers t)


;; projects
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Documents/C/")
  (setq projectile-project-search-path '("~/Documents/C/")))
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-run-use-comint-mode t))


;; if you want to disable splitting the buffer
;;(setq split-width-threshold nil)


(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))


;; for formatting C and C++ code
;; a .clang-format file should be present
;; in projectile directory

(use-package clang-format
  :config
  (setq clang-format-style "file")
  (setq clang-format-fallback-style "LLVM")
  :bind(("C-c C-f" . clang-format-buffer)))


;; lsp + clangd

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-diagnostic-package :none)
  ;; disable side popups
  ;; (setq lsp-ui-sideline-enable nil)
  :hook
  ((c-mode c++-mode) . lsp-mode))

(add-hook 'c-mode-common-hook (lambda() (setq flycheck-checker 'c/c++-clang)(flycheck-mode t)))
	  
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))


(use-package lsp-ivy
  :after lsp)

;;snippets and snippet expansion
(use-package yasnippet-snippets
:hook (prog-mode . yas-minor-mode)
:bind("M-/" . yas-expand))


;; tags for code navigation
(use-package ggtags
:after lsp-mode
:config
(add-hook 'c-mode-common-hook
(lambda ()
(when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
  (ggtags-mode 1)))))

;; completion

;; (use-package auto-complete
;;   :init
;;   (progn
;;     (ac-config-default)
;;     (global-auto-complete-mode t)
;;     (setq ac-sources '(ac-source-yasnippet
;; 		       ac-source-abbrev
;; 		       ac-source-words-in-same-mode-buffers))
;;     ))


;; inserts line below
;; and jumps to it below


(defun end-of-line-and-indented-new-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "<S-return>") 'end-of-line-and-indented-new-line)


(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection)
	      ("C-n" . company-select-next)
	      ("C-r" . company-select-previous))
        (:map lsp-mode-map
              ("<tab>" . company-indent-or-complete-common))	  
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))


;; ;; todo

;; (defun company-yasnippet-or-completion ()
;;   (interactive)
;;   (let ((yas-fallback-behavior nil))
;;     (unless (yas-expand)
;;       (call-interactively #'company-complete-common))))

;;   (add-hook 'company-mode-hook (lambda ()
;;   (substitute-key-definition 'company-complete-common
;;                              'company-yasnippet-or-completion
;;                              company-active-map)))

;; term
(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))


;; set clang as syntax checker for C and C++


;;(global-flycheck-mode t)

;; expand region

(use-package expand-region
  :config
  (global-set-key(kbd "C-=") 'er/expand-region))


;; C/C++ configuration
;; set "gnu" style indenting for C
(setq c-default-style "linux"
      c-basic-offset 4)


;; ------------------ garbage collections and startup performance ----------------

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 2 1000 1000))


;; ---------------------   smart parenthesis ------------------------
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :custom
  (sp-escape-quotes-after-insert t)
  :config
  (require 'smartparens-config)
  :bind
  ("C-k" . 'sp-kill-sexp))

(show-paren-mode t)  

;; (use-package c-or-c++-mode
;;   :config
;;   (progn
;;     (flycheck-mode)
;;     (setq flycheck-checker 'c/c++-clang)
;;     )
;;   )

;; (require 'flycheck)
;; (setq flycheck-checker 'c/c++-clang)
;; (flycheck-mode)
;; (add-hook 'c-mode-common-hook 'flycheck-mode)