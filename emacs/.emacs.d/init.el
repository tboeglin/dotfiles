;;; Save Custom things in a separate file
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file)

;;; STFU and hide unnecessary GUI stuff
(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell t
      inhibit-splash-screen t
      confirm-kill-emacs 'yes-or-no-p
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      tab-always-indent 'complete)

;;; 0 => never blink. n > 0 => blink n times waiting for input
(blink-cursor-mode 0)

(when (display-graphic-p)
  (tool-bar-mode -1)
    ;;; who needs a scrollbar?
  (scroll-bar-mode 0))

(require 'cl)

;; useful command disabled by default
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; local definitions like name and email
(load (expand-file-name "~/.emacs.d/local.el"))

;; Config for builtin text-mode. Used mainly for It's all Text FF extension.
(setq-default ispell-program-name "aspell")
(add-hook 'text-mode-hook (lambda ()
			    (auto-fill-mode 1)
			    ;;(turn-on-filladapt-mode)
			    (flyspell-mode)))

;;; load and setup package.el
(require 'package)
;;; additional repos
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(when (string-equal system-type "darwin")
  (progn
    (setq mac-option-modifier 'super
	  mac-command-modifier 'meta
	  x-select-enable-clipboard t
	  helm-locate-case-fold-search nil
	  helm-locate-command "mdfind %s -name %s"
	  dired-use-ls-dired nil)))
; where homebrew installs stuff
;    (push "/usr/local/bin/" exec-path)))

;; Add the jenv shims dir if it exists so ensime and friends pick up
;; the managed java version
;; (let ((jenv-shims-dir "~/.jenv/shims"))
;;   (when (file-directory-p jenv-shims-dir)
;;     (setq exec-path (cons jenv-shims-dir exec-path))
;;     (setenv "PATH" (concatenate 'string jenv-shims-dir ":" (getenv "PATH")))))

;; (let ((cargo-path "~/.cargo/bin"))
;;   (when (file-directory-p cargo-path)
;;     (setq exec-path (cons cargo-path exec-path))
;;     (setenv "PATH" (concatenate 'string cargo-path ":" (getenv "PATH")))))

;; 					; for stack stuff
;; (push "~/.local/bin" exec-path)
;; (push "~/local/bin" exec-path)
;; ;; make /usr/local/bin visible to process started by call-process.
;; ;; without this, stack (started by intero) cannot find the system
;; ;; wide ghc installed with homebrew
;; (setenv "PATH"
;; 	(concatenate 'string (getenv "PATH") ":/usr/local/bin"))
;; 					; (setenv "ANDROID_HOME" "/usr/local/opt/android-sdk")



;;; ensure use-package is installed and loaded before using it for the
;;; remaining of the configuration
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)

(use-package exec-path-from-shell :ensure :demand
  :config
  (exec-path-from-shell-initialize))

;; (use-package leuven-theme :ensure
;;   :config (load-theme 'leuven)
;;   :demand)

(use-package solarized-theme :ensure
  :config (load-theme 'solarized-dark)
  :demand)

;;; Helm
(use-package helm :ensure :demand
  :config (require 'helm-config)
  (setq helm-quick-update                     t ; do not display invisible candidates
	helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
	helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	helm-ff-file-name-history-use-recentf t)


  (helm-mode 1)
  :bind (("C-x C-f" . helm-find-files)
	 ("M-x" . helm-M-x)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x b" . helm-mini)
	 ("C-h a" . helm-apropos)
	 ("C-x C-b" . helm-buffers-list))
  :diminish "")

					; projectile always activated
(use-package projectile
  :ensure
  :init   (setq projectile-use-git-grep t)
  :config (projectile-global-mode t)
  (setq projectile-completion-system 'helm))

;; used by helm-projectile
(use-package helm-rg :ensure)

					; helm projectile is a separate package
(use-package helm-projectile :ensure :demand
  :config (helm-projectile-on))

;; company completion
(use-package company :ensure
  :diminish ""
  :config (cl-pushnew 'company-irony company-backends)
  (cl-pushnew 'company-tide company-backends)
  (cl-pushnew 'company-anaconda company-backends)
  (setq company-idle-delay 0.5)
  (global-set-key [remap completion-at-point] 'company-complete)
  (global-set-key [C-tab] 'company-complete)
  (add-hook 'after-init-hook 'global-company-mode)
  (global-company-mode 1)
  :bind (:map company-active-map
	      ("C-p" . company-select-previous)
	      ("C-n" . company-select-next)
	      ("C-d" . company-show-doc-buffer)
	      ("M-."  . company-show-location)))

(use-package company-quickhelp :ensure
  :config (company-quickhelp-mode))

;; easier undo
(use-package undo-tree
  :ensure
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind ("C-x u" . undo-tree-visualize))

;;; Org mode
(use-package org :ensure :pin "org"
  :bind (("\C-cl" . org-store-link)
	 ("\C-ca" . org-agenda)
	 ("\C-cc" . org-capture))
  :config (setq org-log-done t
		org-agenda-files (list "~/org/inbox.org"	; where stuff lands
				       "~/org/gtd.org"		; where stuff gets refiled in projects
				       "~/org/someday.org"	; where non immediatly actionnable stuff ends
				       "~/org/ticker.org")	; where stuff 
		org-clock-persist 'history
		org-hierarchical-todo-statistics nil
		org-capture-templates '(("t" "Todo [inbox]" entry
					 (file+headline "~/gtd/inbox.org" "Tasks")
					 "* TODO %i%?")
					("T" "Tickler" entry
					 (file+headline "~/gtd/tickler.org" "Tickler")
					 "* %i%? \n %U"))
		org-refile-targets '(("~/gtd/gtd.org" :maxlevel . 3)
				     ("~/gtd/someday.org" :level . 1)
				     ("~/gtd/tickler.org" :maxlevel . 2))
		org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

  (org-clock-persistence-insinuate))

;;; contains a function to display the current active keymap
(use-package help-fns+ :ensure)

;;; Ace-window for easy window nav
(use-package ace-window :ensure
  :bind ("C-:" . ace-window) 		; azerty
        ("C-." . ace-window))		; qwerty

(use-package ace-jump-mode :ensure
  :bind ("C-c SPC" . ace-jump-mode))

;;; yasnippet
(use-package yasnippet
  :ensure)

;;; magit
(use-package magit
  :ensure
  ;; Prevent vc from handling git repositories
  :config
  (setq vc-handled-backends
	(remove-if (lambda (backend)
		     (eql backend 'Git))
		   vc-handled-backends))
  (setq magit-last-seen-setup-instructions "1.4.0")
  :bind
  ("<f5>" . magit-status)
  ("C-c g s" . magit-status)
  ("C-c g b" . magit-blame))


;;; Common Lisp dev w/ SLIME
(use-package slime
  :ensure
  :init
  (setq inferior-lisp-program "sbcl --dynamic-space-size 2048")
;;	(if (string-equal system-type "darwin") "ccl64" "sbcl"))
  :config
  (set-language-environment "UTF-8")
  (setq slime-net-coding-system 'utf-8-unix)
  (setq slime-use-autodoc-mode nil)
  (slime-setup '(slime-fancy slime-tramp slime-asdf))
  (slime-require :swank-listener-hooks))

;;; Haskell
(use-package intero :ensure)
(use-package haskell-mode :ensure
  :config (add-hook 'haskell-mode-hook 'intero-mode)
  (setq haskell-font-lock-symbols nil))


;;; erlang
(use-package erlang :ensure)

(use-package powerline :ensure
  :config (powerline-default-theme))

(use-package ledger-mode :ensure
  :mode ("\\.dat$" . ledger-mode))

;;; for emacsclient
(use-package server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t))

;;; for rust
(use-package rust-mode :ensure
  :mode "\\.rs\\'"
  :init
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook 'lsp-mode)
  (add-hook 'rust-mode-hook 'lsp-rust-enable))

(use-package lsp-mode :ensure
  :config (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui :ensure)

(use-package lsp-rust :ensure
  :init
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
  :after lsp-mode)


;;; scala stuff
(use-package scala-mode
  :config
  (add-hook 'scala-mode-hook
	    (lambda ()
	      (turn-on-smartparens-strict-mode)
	      (show-paren-mode)
	      (yas-minor-mode)
	      (company-mode)
	      (ensime-mode)
	      (add-to-list 'write-file-functions 'delete-trailing-whitespace))))

(use-package smartparens
  :ensure
  :commands
  smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair

  :init
  (setq sp-interactive-dwim t)

  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (sp-pair "(" ")" :wrap "C-(")
  (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
  (sp-pair "{" "}" :wrap "C-{")
  ;; (sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
  ;; (sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
  (bind-key "s-{" 'sp-rewrap-sexp smartparens-mode-map)

  ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/543
  (bind-key "C-<left>" nil smartparens-mode-map)
  (bind-key "C-<right>" nil smartparens-mode-map)

  (bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
  (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map))

(use-package popup-imenu
  :ensure
  :commands popup-imenu
  :bind ("M-i" . popup-imenu))

(use-package ensime :ensure
  :pin melpa-stable
  :init (setq ensime-startup-snapshot-notification nil
	      ensime-startup-notification nil)
  :config (bind-key "M-p" nil ensime-mode-map)
  (bind-key "C-c C-p" 'ensime-backward-note ensime-mode-map)
  (bind-key "M-n" nil ensime-mode-map)
  (bind-key "C-c C-n" 'ensime-forward-note ensime-mode-map)
  (setq scala-indent:align-parameters t
	scala-indent:align-forms t))

;; Irony
(use-package irony :ensure
  :commands irony-mode
  :init (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony :ensure)

(use-package elfeed :ensure
  :config 
  (global-set-key (kbd "C-x w") 'elfeed)
  (add-hook 'elfeed-new-entry-hook
	    (elfeed-make-tagger :before "2 weeks ago"
				:remove 'unread)))

(use-package tide :ensure
  :commands (tide-mode tide-setup company-tide))


(use-package typescript-mode :ensure
  :config
  (add-hook 'typescript-mode-hook
	    (lambda ()
	      (tide-setup)
	      (flycheck-mode +1)
	      (setq flycheck-check-syntax-automatically '(save mode-enabled))
	      (eldoc-mode +1))))

(use-package yaml-mode :ensure)


;;; Purescript stuff
(use-package psc-ide :ensure)

(use-package purescript-mode :ensure
  :config
  (add-hook 'purescript-mode-hook
	    (lambda ()
	      (psc-ide-mode)
	      (company-mode)
	      (flycheck-mode)
	      (turn-on-purescript-indentation))))

;;; Seldom used scripting languages
;;; Python setup
(use-package python :ensure
  :config (add-hook 'python-mode-hook
		    (lambda ()
		      (add-hook 'write-file-functions 'delete-trailing-whitespace)))
  (add-hook 'python-mode-hook 'anaconda-mode))

;;; ruby stuff
(use-package ruby-mode :ensure)

;;; lua
(use-package lua-mode :ensure)


(use-package anaconda-mode :ensure)
(use-package company-anaconda :ensure)

;;; Stack overflow
(use-package sx :ensure
  :config (bind-keys :prefix "C-c s"
		     :prefix-map my-sx-map
		     :prefix-docstring "Global keymap for SX."
		     ("q" . sx-tab-all-questions)
		     ("i" . sx-inbox)
		     ("o" . sx-open-link)
		     ("u" . sx-tab-unanswered-my-tags)
		     ("a" . sx-ask)
		     ("s" . sx-search)))

(use-package feature-mode :ensure)

(use-package restclient :ensure)
(use-package restclient-helm :ensure)