;;; Code:

(use-package diminish)

(use-package s
  :ensure t)

(use-package makefile-mode
  :mode "Makefile")

;; (use-package symon
;;   :ensure t
;;   :config (progn
;;             (symon-mode)))

(use-package windmove
  :bind (("S-<left>" . windmove-left)
         ("S-<right>" . windmove-right)
         ("S-<up>" . windmove-up)
         ("S-<down>" . windmove-down)))

(use-package nyan-mode
  :ensure t
  :init (nyan-mode 0))

(use-package zone-nyan
  :ensure t)

(use-package emacs-lisp-mode
  :init (progn
	  ;; Recompile if .elc exists.
	  ;;(add-hook (make-local-variable 'after-save-hook)
		;;    (lambda ()
		;;     (byte-force-recompile default-directory)))
	  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
	  ;; (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode) ;; Requires Ispell
	  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
	  (add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
	  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
          (add-hook 'emacs-lisp-mode-hook 'autopair-mode)
	  )

  :config (progn
	    (setq indent-tabs-mode nil)
	    (define-key flyspell-mode-map "\M-\t" nil)
	    (define-key emacs-lisp-mode-map "\r" 'reindent-then-newline-and-indent)
	    (define-key emacs-lisp-mode-map "\C-x\C-e" 'pp-eval-last-sexp)
	    )
  )

(use-package ess
  :ensure t)

(use-package paredit
  :ensure t)

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

;; (use-package shackle
;;   :ensure t
;;   :init
;;   (setq shackle-rules
;; 	'(
;; 	  ("*alchemist test report*" :select nil :size 0.3 :align 'below)
;; 	  ;; ("*Flycheck errors*" :ratio 0.25 :align t :size 0.2)
;; 	  ))
;;   :config (progn
;; 	    (shackle-mode t)
;; 	    )
;;   )

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
	    (use-package ido-completing-read+
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
  :diminish company-mode
  :ensure t
  :init (setq
         company-dabbrev-ignore-case nil
         company-dabbrev-code-ignore-case nil
         company-dabbrev-downcase nil
         company-idle-delay 0
         company-minimum-prefix-length 4)
  :config (progn
            (define-key company-active-map (kbd "C-n") 'company-select-next)
            (define-key company-active-map (kbd "C-p") 'company-select-previous)
            (define-key company-active-map (kbd "C-f") 'company-complete-selection)

            ;; disables TAB in company-mode, freeing it for yasnippet
            (define-key company-active-map (kbd "TAB") nil)
            (define-key company-active-map [tab] nil)

	    (add-hook 'after-init-hook 'global-company-mode)))

(use-package company-quickhelp
  :ensure t
  :config (progn
            (company-quickhelp-mode 1)
))

(use-package git-timemachine
  :ensure t)

(use-package magit
  :ensure t
  :bind (("C-c s" . magit-status)
         ("C-c l" . magit-log)
         ("C-c c" . magit-checkout)
         ("C-c b" . magit-blame))
  :init (progn
	  (setenv "GIT_PAGER" "")
	  (setq magit-completing-read-function 'magit-ido-completing-read)))

;; (use-package smartparens
;;   :ensure t
;;   :diminish smartparens-mode
;;   :config (progn
;; 	    (require 'smartparens-config)
;; 	    (smartparens-global-mode 1)))

(use-package autopair
  :ensure t
  :diminish autopair-mode)

(use-package restclient
  :ensure t)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (progn
	    (which-key-mode)
	    (which-key-setup-minibuffer)
	    ))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (progn
          (yas-global-mode 1))
  :config (progn
            (yas-reload-all)
            (add-hook 'prog-mode-hook #'yas-minor-mode)
            ))

(use-package yatemplate
  :ensure t
  :defer 2 ;; WORKAROUND https://github.com/mineo/yatemplate/issues/3
  :config
  (auto-insert-mode)
  (setq auto-insert-alist nil)
  (yatemplate-fill-alist))

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
          ;; (setq neo-show-hidden-files t)
          (setq neo-autorefresh nil)
          (setq neo-window-width 28)
          (setq neo-hidden-regexp-list '("\\.beam$"))

	  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))

	  (global-set-key (kbd "C-c t") 'neotree-toggle)
	  (global-set-key [f8] 'neotree-toggle)
	  ))

;; (use-package flycheck-elixir
;;   :ensure t)

(use-package elixir-mode
  :ensure t
  :init (progn
	  (add-hook 'elixir-mode-hook 'company-mode)
	  (add-hook 'elixir-mode-hook 'alchemist-mode)
	  ;; (add-hook 'elixir-mode-hook 'elixir-add-electric-pairs)
	  ;; (add-hook 'elixir-mode-hook 'flycheck-elixir)
          (add-hook 'elixir-mode-hook #'yas-minor-mode)
	  (add-hook 'elixir-mode-hook 'flycheck-mode)
	  )
  :config (progn
	    ;; smart parens - reuse some ruby functionality
	    ;; (sp-with-modes '(elixir-mode)
	    ;;   (sp-local-pair "fn" "end"
	    ;;     	     :when '(("SPC" "RET"))
	    ;;     	     :actions '(insert navigate))
	    ;;   (sp-local-pair "do" "end"
	    ;;     	     :when '(("SPC" "RET"))
	    ;;     	     :post-handlers '(sp-ruby-def-post-handler)
	    ;;     	     :actions '(insert navigate)))
	    )

  (use-package flycheck-mix
    :ensure t
    :config (progn
              (flycheck-mix-setup)
              )))

(use-package alchemist
  :ensure t
  ;; :bind (("M-." . alchemist-goto-definition-at-point))
  :config (progn
	    (setq alchemist-goto-erlang-source-dir "~/projects/open-source/otp/")
	    (setq alchemist-goto-elixir-source-dir "~/projects/open-source/elixir/")
	    ;; (setq alchemist-key-command-prefix (kbd "C-c ,")) ;; default: (kbd "C-c a")
	    (setq alchemist-test-display-compilation-output t)
	    (setq alchemist-hooks-test-on-save nil)
	    (setq alchemist-hooks-compile-on-save nil)
            (setq alchemist-mix-test-task "espec")
            (setq alchemist-hooks-compile-on-save t)

	    ;; allows the jumping back out of erlang code
	    (defun custom-erlang-mode-hook ()
	      (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))
	    (add-hook 'erlang-mode-hook 'custom-erlang-mode-hook)
	    ))

(use-package flycheck-dialyxir
  :ensure t)


(use-package ivy-erlang-complete
  :ensure t)

(use-package company-erlang
  :ensure t)

(use-package erlang
  :ensure t
  :init (progn
	  (add-hook 'erlang-mode-hook '(lambda() (setq indent-tabs-mode nil)))
	  (add-hook 'erlang-mode-hook '(lambda () (highlight-symbol-mode 1)))
          (add-hook 'erlang-mode-hook #'company-erlang-init)
          (add-hook 'erlang-mode-hook #'ivy-erlang-complete-init)
          ;; automatic update completion data after save
          (add-hook 'after-save-hook #'ivy-erlang-complete-reparse)
	  )
  :config (progn
	    (require 'erlang-start)
	    (require 'fill-column-indicator)

            (setq ivy-erlang-complete-erlang-root "~/projects/open-source/otp/")

	    (global-flycheck-mode -1)

	    (setq-default fill-column 80)
	    (setq erlang-indent-level 4)

	    (add-to-list 'completion-ignored-extensions ".beam")
	    (add-to-list 'auto-mode-alist '("\\.term\\'" . erlang-mode))
	    (add-to-list 'auto-mode-alist '("\\.terms\\'" . erlang-mode))
	    ))


(use-package scala-mode
  :ensure t
  :interpreter
  ("scala" . scala-mode)
  :init (progn
          (defun scala-comment-blocks ()
            (set (make-local-variable 'comment-start) "/* ")
            (set (make-local-variable 'comment-end) " */")
            (set (make-local-variable 'comment-style) 'multi-line)
            (set (make-local-variable 'comment-empty-lines) t))

          (add-hook 'scala-mode-hook (lambda () (scala-comment-blocks)))
          (add-hook 'scala-mode-hook 'subword-mode)
          )
  :config (progn
            (defun scala-mode-newline-comments ()
              "Custom newline appropriate for `scala-mode'."
              ;; shouldn't this be in a post-insert hook?
              (interactive)
              (newline-and-indent)
              (scala-indent:insert-asterisk-on-multiline-comment))

            (bind-key "RET" 'scala-mode-newline-comments scala-mode-map)))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :commands eldoc-mode)


(use-package erc
  :ensure t
  :commands erc erc-tls
  :init
  (setq
   erc-prompt-for-password nil ;; prefer ~/.authinfo for passwords
   erc-hide-list '("JOIN" "PART" "QUIT")
   erc-autojoin-channels-alist
   '(("irc.freenode.net" "#emacs")
     ("irc.gitter.im" "#ensime/ensime-server" "#ensime/ensime-emacs"))))

(defun gitter()
  "Connect to Gitter."
  (interactive)
  (erc-tls :server "irc.gitter.im" :port 6697))
(defun freenode()
  "Connect to Freenode."
  (interactive)
  (erc :server "irc.freenode.net" :port 6667))

(use-package ensime
  :ensure t
  :pin melpa-stable
  :config (progn
            (add-hook 'ensime-mode-hook 'show-paren-mode)

	    ;; (scala-mode:goto-start-of-code)
	    (global-set-key (kbd "C-<backspace>") 'contextual-backspace)

            (setq
             ensime-sbt-command "/usr/local/bin/sbt"
             sbt:program-name "/usr/local/bin/sbt")

            (setq ensime-search-interface 'ivy)

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

(use-package flycheck-cask
  :ensure t
  :commands flycheck-cask-setup
  :config (add-hook 'emacs-lisp-mode-hook (flycheck-cask-setup)))

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


(use-package tide
  :ensure t
  :config (progn
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode +1)
            (tide-hl-identifier-mode +1)
            ;; company is an optional dependency. You have to
            ;; install it separately via package-install
            ;; `M-x package-install [ret] company`
            (company-mode +1)

            ;; aligns annotation to the right hand side
            (setq company-tooltip-align-annotations t)

            ;; formats the buffer before saving
            (add-hook 'before-save-hook 'tide-format-before-save)

            (add-hook 'typescript-mode-hook #'setup-tide-mode)

            (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

            ))

(use-package js2-mode
  :mode ".js"
  :ensure t
  :config (progn
            (add-hook 'js2-mode-hook #'tide-setup)
            ))

(use-package clojure-mode
  :mode "\\.clj\\'"
  :config (progn
	    (use-package cider
	      :mode "\\.clj\\'")
	    ))

(use-package counsel
  :ensure t
  :bind(
        ("M-x" . counsel-M-x)
        ("M-y" . counsel-yank-pop)
        ("C-x C-f" . counsel-find-file)
        ("M-i" . counsel-imenu)
        ("C-c g" . counsel-git)
        ("C-c j" . counsel-git-grep)
        ("C-c k" . counsel-ag)
        ("C-c l" . counsel-locate)
        ("C-c h f" . counsel-describe-function)
        ("C-c h v" . counsel-describe-variable)
        ("C-c h s" . counsel-info-lookup-symbol)
        ("C-c i u" . counsel-unicode-char)
        ("<f1> f" . counsel-describe-function)
        ("<f1> v" . counsel-describe-variable)
        ("<f1> l" . counsel-find-library)
        ("<f2> i" . counsel-info-lookup-symbol)
        ("<f2> u" . counsel-unicode-char)
        ))

(use-package swiper
  :ensure t
  :bind (
         ("C-s" . swiper)
         ))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (
         ("M-y" . ivy-next-line)
         )
  :config (progn
            (ivy-mode 1)

            (setq ivy-use-virtual-buffers t)
            (setq ivy-count-format "(%d/%d) ")
            (setq ivy-re-builders-alist
                  '((t . ivy--regex-plus)))
            ))

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind ("s-/" . undo-tree-visualize))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-on))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x o") 'ace-window))

(use-package tramp
  :config
  (setq tramp-verbose 9
        tramp-default-method "ssh"
        tramp-ssh-controlmaster-options
        (concat "-o ControlPath=/tmp/tramp.%%r@%%h:%%p "
                "-o ControlMaster=auto "
                "-o ControlPersist=no")))

(use-package haskell-mode
  :ensure t)

(use-package intero
  :ensure t
  :init (progn
          (add-hook 'haskell-mode-hook 'intero-mode)
          ))

(use-package bm
  :ensure t
  :config (progn

            (defun bm-counsel-get-list (bookmark-overlays)
              (-map (lambda (bm)
                      (with-current-buffer (overlay-buffer bm)
                        (let* ((line (replace-regexp-in-string "\n$" "" (buffer-substring (overlay-start bm)
                                                                                          (overlay-end bm))))
                               ;; line numbers start on 1
                               (line-num (+ 1 (count-lines (point-min) (overlay-start bm))))
                               (name (format "%s:%d - %s" (buffer-name) line-num line))
                               )

                          `(,name . ,bm)
                          )
                        )
                      )
                    bookmark-overlays))

            (defun bm-counsel-find-bookmark ()
              (interactive)

              (let* ((bm-list (bm-counsel-get-list (bm-overlays-lifo-order t)))
                     (bm-hash-table (make-hash-table :test 'equal))
                     (search-list (-map (lambda (bm) (car bm)) bm-list)))

                (-each bm-list (lambda (bm)
                                 (puthash (car bm) (cdr bm) bm-hash-table)
                                 ))

                (ivy-read "Find bookmark: "
                          search-list
                          :require-match t
                          :keymap counsel-describe-map
                          :action (lambda (chosen)
                                    (let ((bookmark (gethash chosen bm-hash-table)))
                                      (switch-to-buffer (overlay-buffer bookmark))
                                      (bm-goto bookmark)
                                      ))
                          :sort t
                          )))

            ))

(provide 'init-packages)
;;; init-packages.el ends here
