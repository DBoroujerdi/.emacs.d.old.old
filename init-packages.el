;;; Code:

(use-package diminish)

(use-package s
  :ensure t)

(use-package makefile-mode
  :mode "Makefile")

(use-package symon
  :ensure t
  :config (progn
            (symon-mode)))

(use-package windmove
  :bind (("S-<left>" . windmove-left)
         ("S-<right>" . windmove-right)
         ("S-<up>" . windmove-up)
         ("S-<down>" . windmove-down)))

(use-package nyan-mode
  :ensure t
  :init (nyan-mode 1))

(use-package zone-nyan
  :ensure t)

(use-package emacs-lisp-mode
  :init (progn
	  ;; Recompile if .elc exists.
	  (add-hook (make-local-variable 'after-save-hook)
		    (lambda ()
		      (byte-force-recompile default-directory)))
	  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
	  ;; (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode) ;; Requires Ispell
	  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
	  (add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
	  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
	  )

  :config (progn
	    (setq indent-tabs-mode nil)
	    (define-key flyspell-mode-map "\M-\t" nil)
	    (define-key emacs-lisp-mode-map "\r" 'reindent-then-newline-and-indent)
	    (define-key emacs-lisp-mode-map "\C-x\C-e" 'pp-eval-last-sexp)
	    )
  )

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-e C-S-e" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("S-<f6>" . mc/mark-all-like-this)))

;; This package highlights the cursor every time it jumps abruptedly from a
;; place to another (e.g. when changing windows and so on).
(use-package beacon
  :ensure t
  :defer 2
  :diminish beacon-mode
  :config
  (beacon-mode 1))

(use-package drag-stuff
  :ensure t
  :diminish drag-stuff-mode
  :bind (("M-<down>" . drag-stuff-down)
         ("M-<up>" . drag-stuff-up))
  :config
  (drag-stuff-global-mode))

(use-package gitignore-mode
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :init (progn
	  (when (memq window-system '(mac ns x))
	    (exec-path-from-shell-initialize))
	  ))

(use-package shackle
  :ensure t
  :init
  (setq shackle-rules
	'(
	  ("*alchemist test report*" :select nil :size 0.3 :align 'below)
	  ("*Flycheck errors*" :ratio 0.25 :align t :size 0.2)
	  ))
  :config (progn
	    (shackle-mode t)
	    )
  )

(use-package rainbow-delimiters
  :ensure t)

(use-package fill-column-indicator
  :ensure t)

(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol-mode
  :commands highlight-symbol)

(use-package smex
  :ensure t
  :config (progn
	    (smex-initialize)

	    (global-set-key (kbd "M-x") 'smex)
	    (global-set-key (kbd "M-X") 'smex-major-mode-commands)
	    (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
	    ))

(use-package ido
  :init (progn
	  (ido-mode 1)
	  (ido-vertical-mode 1)
	  (ido-everywhere 1)
	  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
	  (setq ido-enable-flex-matching t)
	  (setq ido-ignore-extensions t)
	  )
  :config (progn
	    (use-package ido-ubiquitous
	      :ensure t)

	    (use-package ido-vertical-mode
	      :ensure t)
	    ))

(use-package command-log-mode
  :ensure t
  :config (progn
	    (require 'command-log-mode)
	    ))

(use-package avy
  :ensure t
  :bind (("C-." . avy-goto-word-or-subword-1)
         ("C-," . avy-goto-char))
  :config (progn
            (setq avy-all-windows t)
            (setq avy-styles-alist '((avy-goto-char-2 . post)))
            (setq avy-background t)))

(use-package docker
  :ensure t
  :disabled
  :config (progn
	    (docker-global-mode 1)))

(use-package popup-imenu
  :ensure t
  :commands popup-imenu
  :bind ("M-i" . popup-imenu))

(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :config
  (setq git-gutter-fr:side 'left-fringe)
  (global-git-gutter-mode t))

(use-package company
  :ensure t
  :config (progn
	    (add-hook 'after-init-hook 'global-company-mode)))

(use-package magit
  :ensure t
  :bind (("C-c s" . magit-status)
         ("C-c l" . magit-log)
         ("C-c c" . magit-checkout)
         ("C-c b" . magit-blame))
  :init (progn
	  (setenv "GIT_PAGER" "")
	  (setq magit-completing-read-function 'magit-ido-completing-read)))


(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config (progn
	    (require 'smartparens-config)
	    (smartparens-global-mode 1)))

(use-package magit
  :ensure t
  :config (progn
	    (setenv "GIT_PAGER" "")
	    (setq magit-completing-read-function 'magit-ido-completing-read)))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (progn
	    (which-key-mode)
	    (which-key-setup-minibuffer)
	    ))

(use-package yasnippet
  :ensure t
  :defer 4
  :diminish yas-minor-mode
  :config (progn
	    (global-unset-key (kbd "s-e"))
	    (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
	    (define-key yas-minor-mode-map (kbd "<tab>") nil)
	    (define-key yas-minor-mode-map (kbd "TAB") nil)
	    (define-key yas-minor-mode-map (kbd "s-e") 'yas-expand)
	    (yas-global-mode t)))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (progn
	    (projectile-global-mode 1)
	    (setq projectile-switch-project-action 'projectile-dired)
	    (setq projectile-switch-project-action 'neotree-projectile-action)
	    ))

(use-package flycheck
  :ensure t
  :init (progn
	  (global-flycheck-mode t)

	  ;; (add-to-list 'display-buffer-alist
	  ;; 	       `(,(rx bos "*Flycheck errors*" eos)
	  ;; 		 (display-buffer-reuse-window
	  ;; 		  display-buffer-in-side-window)
	  ;; 		 (side            . bottom)
	  ;; 		 (reusable-frames . visible)
	  ;; 		 (window-height   . 0.15)))
	  ))

(use-package neotree
  :ensure t
  :init (progn
	  (setq neo-smart-open nil)
          (setq neo-show-hidden-files t)
          (setq neo-autorefresh nil)
          (setq neo-window-width 28)

	  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))

	  (global-set-key (kbd "C-c t") 'neotree-toggle)
	  (global-set-key [f8] 'neotree-toggle)
	  ))

;; (use-package flycheck-mix
;;   :ensure t)

(use-package flycheck-elixir
  :ensure t)

(use-package elixir-mode
  :ensure t
  :init (progn
	  (add-hook 'elixir-mode-hook 'company-mode)
	  (add-hook 'elixir-mode-hook 'alchemist-mode)
	  ;; (add-hook 'elixir-mode-hook 'elixir-add-electric-pairs)
	  ;; (add-hook 'elixir-mode-hook 'flycheck-elixir)
	  (add-hook 'elixir-mode-hook 'flycheck-mode)
	  )
  :config (progn
	    ;; smart parens - reuse some ruby functionality
	    (sp-with-modes '(elixir-mode)
	      (sp-local-pair "fn" "end"
			     :when '(("SPC" "RET"))
			     :actions '(insert navigate))
	      (sp-local-pair "do" "end"
			     :when '(("SPC" "RET"))
			     :post-handlers '(sp-ruby-def-post-handler)
			     :actions '(insert navigate)))
	    ))

(use-package flycheck-mix
  :ensure t
  :config (progn
	    (flycheck-mix-setup)
	    ))

(use-package alchemist
  :ensure t
  :config (progn
	    (setq alchemist-goto-erlang-source-dir "~/projects/open-source/otp/")
	    (setq alchemist-goto-elixir-source-dir "~/projects/open-source/elixir/")
	    ;; (setq alchemist-key-command-prefix (kbd "C-c ,")) ;; default: (kbd "C-c a")
	    (setq alchemist-test-display-compilation-output t)
	    (setq alchemist-hooks-test-on-save nil)
	    (setq alchemist-hooks-compile-on-save nil)

	    ;; allows the jumping back out of erlang code
	    (defun custom-erlang-mode-hook ()
	      (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))
	    (add-hook 'erlang-mode-hook 'custom-erlang-mode-hook)
	    ))

(use-package flycheck-dialyxir
  :ensure t)

(use-package elixir-yasnippets
  :ensure t)

(use-package erlang
  :ensure t
  :init (progn
	  (add-hook 'erlang-mode-hook '(lambda() (setq indent-tabs-mode nil)))
	  (add-hook 'erlang-mode-hook '(lambda () (highlight-symbol-mode 1)))
	  )
  :config (progn
	    (require 'erlang-start)
	    (require 'fill-column-indicator)

	    (global-flycheck-mode -1)

	    (setq-default fill-column 80)
	    (setq erlang-indent-level 4)

	    (add-to-list 'completion-ignored-extensions ".beam")
	    (add-to-list 'auto-mode-alist '("\\.term\\'" . erlang-mode))
	    (add-to-list 'auto-mode-alist '("\\.terms\\'" . erlang-mode))
	    ))

(use-package ensime
  :disabled
  :mode "\\.scala\\'"
  :ensure t
  :pin melpa-stable
  :config (progn
	    (scala-mode:goto-start-of-code)
	    (global-set-key (kbd "C-<backspace>") 'contextual-backspace)

	    (defun contextual-backspace ()
	      "Hungry whitespace or delete word depending on context."
	      (interactive)
	      (if (looking-back "[[:space:]\n]\\{2,\\}" (- (point) 2))
		  (while (looking-back "[[:space:]\n]" (- (point) 1))
		    (delete-char -1))
		(cond
		 ((and (boundp 'smartparens-strict-mode)
		       smartparens-strict-mode)
		  (sp-backward-kill-word 1))
		 ((and (boundp 'subword-mode)
		       subword-mode)
		  (subword-backward-kill 1))
		 (t
		  (backward-kill-word 1)))))
	    ))

(use-package go-mode
  :ensure t
  :init
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)
    (add-hook 'go-mode-hook #'global-flycheck-mode)
    (add-hook 'go-mode-hook 'electric-pair-mode)
    (add-hook 'go-mode-hook 'go-eldoc-setup)

    (add-hook 'go-mode-hook (lambda ()
                              (set (make-local-variable 'company-backends) '(company-go))
                              (company-mode))))

  :config
  (progn
    (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
    (local-set-key (kbd "C-c C-g") 'go-goto-imports)
    (local-set-key (kbd "C-c C-k") 'godoc)

    (use-package go-eldoc
      :ensure t
      :defer
      :init
      (add-hook 'go-mode-hook 'go-eldoc-setup))
    ))

(use-package clojure-mode
  :mode "\\.clj\\'"
  :config (progn
	    (use-package cider
	      :mode "\\.clj\\'")
	    ))

(use-package counsel
  :ensure t
  :bind
  (("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))


(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("C-x C-f" . counsel-find-file)
         ("M-i" . counsel-imenu)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-c l" . counsel-locate)
         ("C-c h f" . counsel-describe-function)
         ("C-c h v" . counsel-describe-variable)
         ("C-c i u" . counsel-unicode-char)
         ))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-on))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x o") 'ace-window))


(provide 'init-packages)
;;; init-packages.el ends here
