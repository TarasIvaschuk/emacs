;; -------- customoe variables -----------

;; You will most likely need to adjust this font size for your system!
(defvar my/default-font-size 130)
(defvar my/default-variable-font-size 130)
;; Make frame transparency overridable
(defvar my/frame-transparency '(100 . 100))
;; (defvar my/font-face "Inconsolata Nerd Font Mono")
(defvar my/font-face "SourceCode Pro")
;; (defvar my/font-face "Hack Nerd Font Mono")
;; (defvar my/font-face "Iosevka Comfy Fixed")
;; (defvar my/font-face "Iosevka Nerd Font Mono")
(defvar my/font-weight 'regular)

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

(setq auto-save-default nil)
(setq make-backup-files nil)


;;------------------- auto save files --------------------

(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))



;; -----------------  basic ui ---------------------

(repeat-mode 1)
(recentf-mode 1)


;; to keep point position while scrolling
(setq scroll-preserve-screen-position t)

;; ctrl+ backspace removes a space or word
;; https://stackoverflow.com/questions/28221079/ctrl-backspace-in-emacs-deletes-too-much

(defun my/backward-kill-word ()
  "Remove all whitespace if the character behind the cursor is whitespace, otherwise remove a word."
  (interactive)
  (if(looking-back "[ \t\n]")
      ;; delete horizontal space before us and then check to see if we
      ;; are looking at a newline
      (progn (delete-horizontal-space 't)
             (while(looking-back "[ \t\n]")
               (backward-delete-char 1)))
    ;; otherwise, just do the normal kill word.
      (backward-kill-word 1)))

(global-set-key (kbd "C-<backspace>") 'my/backward-kill-word)

;;duplicate line
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

(global-set-key (kbd "C-S-d") 'duplicate-line)

;; remap M-n and M-r in Man mode such way disabling interfering with smartscan keybindings
(add-hook 'Man-mode-hook
	  (lambda ()
(keymap-set Man-mode-map "M-N" 'Man-next-manpage)
(keymap-set Man-mode-map "M-P" 'Man-previous-manpage)))


;; query-replace current word
(defun qrc (replace-str)
  (interactive "sDo query-replace current word with: ")
  (let ((word (thing-at-point 'symbol)))
  (beginning-of-thing 'symbol)
      (query-replace word replace-str)))
  
;; delete selection mode
(delete-selection-mode 1)

;; increase line spacing
;; (setq-default line-spacing 0.1)

;; select text using a mouse without dragging
;; https://superuser.com/questions/521223/shift-click-to-extend-marked-region

(define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill)

;; dired
(setq dired-dwim-target t)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; jump quickly on symbol
(use-package smartscan
  :defer 1
  :config
  (global-smartscan-mode 1))

(global-unset-key (kbd "<insert>"))

;; save the cursor's last place in buffer
(save-place-mode t)

;; display a counter showing the number of the current and other
;; matches. Place it before prompt (or after if needed)

(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)

;; display line numbers nicely
;; do not forget to turn off display-line-numbers mode
;; M-b customize-option RET and toggle it to nil
;; save and apply

(use-package nlinum
  :custom
  (nlinum-format "%4d     ")
  (nlinum-highlight-current-line t)
  (nlinum-widen t)
  :config
  (global-nlinum-mode t))


;;to stop startup message
(setq inhibit-startup-message t)

(set-fringe-mode '(0 . 0))      ; no  breathing room

;; select a line
(defun my/select-current-line-and-forward-line (arg)
  "Select the current line and move the cursor by ARG lines IF
no region is selected.

If a region is already selected when calling this command, only move
the cursor by ARG lines."
  (interactive "p")
  (when (not (use-region-p))
    (forward-line 0)
    (set-mark-command nil)
    (move-end-of-line arg)))
;; (global-set-key (kbd "C-l") #'my/select-current-line-and-forward-line)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha my/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,my/frame-transparency))

;; maximized mode
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(defun my/set-font-faces ()
  (message "Setting faces!")
  (set-face-attribute 'default nil :font my/font-face :height my/default-font-size :weight my/font-weight)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font my/font-face :height my/default-font-size :weight my/font-weight)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font my/font-face :height my/default-variable-font-size :weight my/font-weight))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                ;; (setq doom-modeline-icon t)
                (with-selected-frame frame
                  (my/set-font-faces))))
    (my/set-font-faces))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; when type std:: 
;; the cursor jumps to the beginning of the line and it gets
;; so annoying
			
;; make electric-pair-mode work on more brackets
;; (setq electric-pair-pairs
;;       '(
;;         (?\" . ?\")
;;         (?\{ . ?\})
;;         (?\' . ?\')))


;;(setq-default linum-format "%4d  ")

(column-number-mode)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
		xref--xref-buffer-mode-hook
		occur-hook
		Man-mode-hook))
  (add-hook mode (lambda () (nlinum-mode 0))))

;;(global-display-line-numbers-mode t)

;;highlight the line
(global-hl-line-mode t)

;;enable clipboard
(setq select-enable-clipboard t)

;; ----------------------------  UI Configuration --------------------
;; custom load path for themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/modus-themes")

(require 'modus-themes)

;; Add all your customizations prior to loading the themes
(setq modus-themes-italic-constructs t  modus-themes-bold-constructs t modus-themes-disable-other-themes t)
;; Maybe define some palette overrides, such as by using our presets
;;(setq modus-themes-common-palette-overrides
 ;;     `((bg-paren-match bg-magenta-intense),@modus-themes-preset-overrides-cooler))
;; (setq modus-themes-common-palette-overrides '((bg-paren-match bg-magenta-intense)))


;; Load the theme of your choice.
(load-theme 'modus-operandi t)
(define-key global-map (kbd "<f5>") #'modus-themes-toggle)


;; (setq modus-themes-italic-constructs t
;;       modus-themes-bold-constructs t)

;; (setq modus-themes-operandi-palette-overrides modus-themes-preset-overrides-faint)
;; Load the theme of your choice:
;; (load-theme 'modus-operandi t) ;; OR (load-theme 'modus-vivendi)

;; Enable underlines by applying a color to them
;; (setq modus-themes-common-palette-overrides
;;       '((bg-paren-match bg-magenta-intense)
;;         (underline-paren-match fg-main)))

;; (global-set-key (kbd "<f5>") 'compile)

;; (setq modus-themes-paren-match '(bold intense))
;;(setq modus-themes-syntax 'yellow-comments)

;;(setq modus-themes-syntax '(alt-syntax yellow-comments)) 

;; to navigate in a buffer by letters

(use-package avy
  :after (lsp-mode)
  :bind
  ("C-c a" . avy-goto-char-timer)
  :init
  (setq avy-timeout-seconds 2))

;; colorize the brackets

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-themes)

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 3))

;; --------------------- completion framework ----------------------------
(defun dw/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
      (backward-kill-word arg)))

(use-package vertico
  :bind (:map vertico-map
         ("C-n" . vertico-next)
         ("C-r" . vertico-previous)
         ("C-f" . vertico-exit)
         :map minibuffer-local-map
         ("<C-backspace>" . dw/minibuffer-backward-kill))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))
					)))

(defun dw/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(use-package consult
  :after vertico
  ;; :bind (("C-s" . consult-line)
         ;; ("C-M-l" . consult-imenu)
         ;; ("C-M-j" . persp-switch-to-buffer*)
         ;; :map minibuffer-local-map
         ;; ("C-r" . consult-history))
  :custom
  (consult-project-root-function #'dw/get-project-root)
  (completion-in-region-function #'consult-completion-in-region))


(use-package consult-projectile
  :after (consult projectile))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
(marginalia-mode))

(use-package embark
  :bind (("C-S-a" . embark-act)))

(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
  '(embark-which-key-indicator
    embark-highlight-indicator
    embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)



;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ------------------------------ end of completion framework ---------------------------------------
;; (use-package helpful
;;   :commands (helpful-callable helpful-variable helpful-command helpful-key)
;;   :bind
;;   ([remap describe-command] . helpful-command)
;;   ([remap describe-key] . helpful-key))

;; ---------------------  leader keybindings ----------------------
(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; to use shift+ space as prefix to global
;; keybindings  
(use-package general
  :config
  (general-auto-unbind-keys)
  (general-create-definer taras/my-leader-keys
    :prefix "C-;"))

  (taras/my-leader-keys
    "t" '(load-theme :which-key "choose a theme")
    "w" '(other-window :which-key "other window")
    "i" '(consult-imenu : which-key "consult-imenu")
    "k" '(kill-this-buffer :which-key "kill this buffer")
    "SPC" '(mode-line-other-buffer :which-key "switch to recent buffer")
    "q" '(qrc :which-key "query-replace the word at point")
    "s" '(my/select-current-line-and-forward-line :which-key "select-current-line"))


;; folding
(use-package origami
  :hook
  (c-mode . origami-mode))

  (taras/my-leader-keys
    "o"  '(:ignore t :which-key "origami")
    "oo" '(origami-open-node-recursively :which-key "recursively open")
    "oc" '(origami-close-node-recursively :which-key "recursively close")
    "oa" '(origami-toggle-all-nodes :which-key "toggle all"))


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
  ;;:custom ((projectile-completion-system 'vertico))
  :bind-keymap
  ("C-c p" . projectile-command-map)
 :bind					
  ([remap projectile-switch-project] . consult-projectile-switch-project)
  ([remap projectile-switch-to-buffer] . consult-projectile-switch-to-buffer)
  ([remap projectile-find-file] . consult-projectile-find-file)
  ([remap projectile-recentf] . consult-projectile-recentf)
  ([remap projectile-find-file-other-window] . consult-projectile-find-file-other-window)
  ([remap projectile-find-dir] . consult-projectile-find-dir)
  ([remap projectile-switch-to-buffer-other-window] . consult-projectile-switch-to-buffer-other-window)
  ([remap projectile-ripgrep] . consult-ripgrep)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects")
  (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-run-use-comint-mode t))


;; if you want to disable splitting the buffer
;;(setq split-width-threshold nil)

(use-package projectile-ripgrep
  :after projectile)

(use-package wgrep
  :after projectile)

;; ------------------------lsp  -----------------------------

;; disable inserting headers automatically
;;https://emacs.stackexchange.com/questions/58015/how-to-stop-lsp-mode-including-headers-automatically-for-c-c-code
(setq lsp-clients-clangd-args
    '("--header-insertion=never"))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-diagnostic-package :none))

	  
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)
  :custom
  (lsp-ui-doc-position 'at-point))

;; (use-package consult-lsp
;;    :after lsp)

(taras/my-leader-keys
  "e" '(eldoc-print-current-symbol-info :which-key "print eldoc"))
  ;; "d" 'xref-find-definitions
  ;; "lr" 'xref-find-references
  ;; "ln" 'lsp-ui-find-next-reference
;;   "lp" 'lsp-ui-find-prev-reference
;;   "le" 'flycheck-list-errors
;;   "lS" 'lsp-ui-sideline-mode
;;   "lX" 'lsp-execute-code-action)


(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))


;; for formatting C and C++ code
;; a .clang-format file should be present
;; in projectile directory

(use-package clang-format
  :config
  (setq clang-format-style "file")
  (setq clang-format-fallback-style "LLVM"))


(taras/my-leader-keys
  "f" '(clang-format-buffer :which-key "format buffer with clang"));

;; ------------------------ company , snippets  -----------------------------

;;snippets expansion
(use-package yasnippet-snippets
:hook (prog-mode . yas-minor-mode))

;; tags for code navigation
;; (use-package ggtags
;; :after lsp-mode
;; :config
;; (add-hook 'c-mode-common-hook
;; (lambda ()
;; (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;   (ggtags-mode 1)))))

;; opens eshell in right split window
;;(add-to-list 'display-buffer-alist
           ;; '("^\\*eshell\\*$" (display-buffer-in-side-window) (side . bottom)))

;; use hydra to resize buffers
(use-package hydra)

(defhydra hydra-buffer-resize (:timeout 6)
  "resize buffer"
  ("h" enlarge-window-horizontally "enlarge h")
  ("s" shrink-window-horizontally "shrink h")
  ("d" shrink-window "shrink v")
  ("u" enlarge-window "enlarge v")
  ("f" nil "finished" :exit t))

(taras/my-leader-keys
 "r" '(hydra-buffer-resize/body :which-key "resize buffer"))

;; inserts line below
;; and jumps to it below
(defun end-of-line-and-indented-new-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key (kbd "<S-return>") 'end-of-line-and-indented-new-line)


(use-package company
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection)
	      ("C-n" . company-select-next)
	      ("C-r" . company-select-previous))
        ;; (:map lsp-mode-map
        ;;       ("<tab>" . company-indent-or-complete-common))	  
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


;; expand region
(use-package expand-region
  :config
  (global-set-key(kbd "C-=") 'er/expand-region)
  (global-set-key(kbd "C-+") 'er/contract-region))


;; ------------------ garbage collections and startup performance ----------------

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 2 1000 1000))


;; ---------------------   smart parenthesis ------------------------
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :custom
  (sp-escape-quotes-after-insert t)
  :config
  (require 'smartparens-config))

(show-paren-mode t)  

;;--------- C style -------------

(use-package flycheck
  :after lsp)

;; (add-hook 'c-mode-hook #'lsp)
;; (add-hook 'c++-mode-hook #'lsp)

;; remove electric behaviour( reindentation) when pressing : in c++
;; https://www.reddit.com/r/emacs/comments/4iuw9s/wanting_to_disable_electric_indenting_except_on/
;; https://stackoverflow.com/questions/24097839/how-to-add-include-path-to-flycheck-c-c-clang
(defun my-c-common-mode-hook ()
  (lsp)
  (define-key c-mode-map ":" 'self-insert-command)
  (define-key c++-mode-map ":" 'self-insert-command)
  (setq comment-start "//" comment-end "")
  (require 'flycheck)
  (setq flycheck-checker 'c/c++-clang)
  (setq flycheck-clang-include-path (list (expand-file-name "~/taras-ivashchuk-fork/http_server/mbedtls/include/")
					  (expand-file-name "~/taras-ivashchuk-fork/http_server/mbedtls/tests/include/")))
  (flycheck-mode t)
  (flymake-mode -1)
  (setq lsp-language-server 'clangd))

(add-hook 'c-mode-common-hook 'my-c-common-mode-hook)
(setq c-default-style "linux" c-basic-offset 4)


;; https://emacs.stackexchange.com/questions/4119/problem-assigning-variables
;; https://stackoverflow.com/questions/18612742/emacs-desktop-save-mode-error#comment47963002_26546872
(setq desktop-path `(,user-emacs-directory "~"))
;;(add-hook 'server-after-make-frame-hook 'desktop-read)
(setq desktop-restore-forces-onscreen nil)


;; ---------------------------    eglot ----------------------------------
;; https://clangd.llvm.org/installation
;; (require 'eglot)
;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
;; (add-hook 'c-mode-hook (lambda() (eglot-ensure) ))
;; (add-hook 'eglot-managed-mode-hook (lambda ()(eglot-inlay-hints-mode -1)(eldoc-mode -1)  (message "eglot hints fixed")))
