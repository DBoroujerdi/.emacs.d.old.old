
;;
;; Bootstrap
;;

(require 'package)

(setq package-archives
      (append package-archives
              '(("melpa" . "http://melpa.milkbox.net/packages/"))))

(package-initialize)
(setq package-enable-at-startup nil)(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))


;; If never connected to repositories before, download package descriptions so
;; `use-package' can trigger installation of missing packages.
(unless package-archive-contents
  (message "Refreshing ELPA package archives...")
  (package-refresh-contents))


;; ...but before everything, make sure `use-package' is installed.
(unless (package-installed-p 'use-package)
  (message "`use-package' not found.  Installing...")
  (package-install 'use-package))

(require 'use-package)
(setq use-package-minimum-reported-time 0
      use-package-verbose t)

(message "Running as user %s .."
	 ((lambda ()
	    (getenv
	     (if (equal system-type 'windows-nt) "USERNAME" "USER")))))


;;
;; Config
;;

;; This makes my Emacs startup time ~35% faster.
(setq gc-cons-threshold 100000000)

;; inserts newline on C-n when on last line in the buffer
(setq next-line-add-newlines t)

;; prefer spaces over tabs
(setq-default indent-tabs-mode nil)

;; typed text replaces selected
(delete-selection-mode 1)

;; Cmd as meta
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable toolbar
(tool-bar-mode -1)

;; solid cursor
(blink-cursor-mode 0)

;; disable scroll bar
(scroll-bar-mode -1)

;; no tool bar
(tool-bar-mode 0)

;; no menu bar
(menu-bar-mode 0)

;; no default start up screen
(setq inhibit-startup-screen t)

;; no initial scratch text
(setq initial-scratch-message nil)

;; buffer line spacing
(setq-default line-spacing 5)

;; display line numbers
;; (setq linum-format "%d ")
;; (global-linum-mode t)

;; window size
(setq initial-frame-alist '((top . 0) (left . 0) (width . 300) (height . 80)))
;; new window sizes
(setq default-frame-alist '((top . 0) (left . 0) (width . 300) (height . 80)))
;; width  -> num characters
;; height -> num lines

;; limit the number of times a frame can split
(setq split-width-threshold 200)

(setq c-default-style "linux" c-basic-offset 8)

(set-frame-font "Inconsolata 13" t t)

;; for new frames and emacs client..
(setq default-frame-alist '((font . "Inconsolata 13")))

;; set font size
(set-face-attribute 'default nil :height 120)

;; delete trailing whitespace on save action
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Save here instead of littering current directory with emacs backup files
(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))

;; Mousewheel scrolling can be quite annoying, lets fix it to scroll smoothly.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;;
;; Load other modules
;;

(load "~/.emacs.d/init-functions")
(load "~/.emacs.d/init-keys")
(load "~/.emacs.d/init-packages")


;;
;; Themes
;;

(use-package solarized-theme :ensure t)
(use-package arjen-grey-theme :ensure t)

(load-theme 'arjen-grey t)


;;
;; Generated code
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode go-eldoc go-mode shackle beacon drag-stuff gitignore-mode erlang elixir-yasnippets flycheck-dialyxir alchemist flycheck-elixir flycheck-mix neotree flycheck projectile which-key smartparens magit company git-gutter popup-imenu ido-vertical-mode ido-ubiquitous command-log-mode smex highlight-symbol fill-column-indicator solarized-theme rainbow-delimiters exec-path-from-shell all-the-icons use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
