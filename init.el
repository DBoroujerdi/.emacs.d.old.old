
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


;; straight - for checking out packages with git, for local hacking and contributing
;; (let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
;;       (bootstrap-version 2))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

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

(if (eq system-type 'darwin)
    (progn
      ;; Cmd as meta
      (setq mac-option-key-is-meta nil)
      (setq mac-command-key-is-meta t)
      (setq mac-option-modifier 'super) ; make opt key do Super
      (setq mac-command-modifier 'meta)
      ))

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
;; (setq initial-frame-alist '((top . 0) (left . 0) (width . 300) (height . 80)))
;; new window sizes
;; (setq default-frame-alist '((top . 0) (left . 0) (width . 300) (height . 80)))
;; width  -> num characters
;; height -> num lines


(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 200) ; chars
              (height . 60) ; lines
              ))

      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 200)
              (height . 60)
              )))
  (progn
    (setq initial-frame-alist
          '(
            (tool-bar-lines . 0)))
    (setq default-frame-alist
          '(
            (tool-bar-lines . 0)))))


;; limit the number of times a frame can split
(setq split-width-threshold 200)

(setq c-default-style "linux" c-basic-offset 8)

;; for new frames and emacs client..
;; (setq default-frame-alist '((font . "DejaVu Sans Mono")))

(defun set-default-font-if-exists (font)
  (if (x-list-fonts font)
      (set-default-font font)
    ))

;; set font size
(set-face-attribute 'default nil :height 110)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(when (window-system)
  (set-default-font-if-exists "Fira Code"))

(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

;; delete trailing whitespace on save action
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Save here instead of littering current directory with emacs backup files
(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))

;; Mousewheel scrolling can be quite annoying, lets fix it to scroll smoothly.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)


;; suppress bell sounds
(setq ring-bell-function (progn ))
(setq visible-bell nil)


(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda ()
                   (flyspell-mode 1)
                   (visual-line-mode 1)
                   )))

;;
;; Load other modules
;;

(load "~/.emacs.d/init-functions")
(load "~/.emacs.d/init-keys")
(load "~/.emacs.d/init-packages")


;;
;; Themes
;;

;; (use-package solarized-theme :ensure t)
;; (use-package arjen-grey-theme :ensure t)
(use-package doom-themes :ensure t)

(load-theme 'doom-vibrant t)


;;
;; Generated code
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("e1994cf306356e4358af96735930e73eadbaf95349db14db6d9539923b225565" "0f0022c8091326c9894b707df2ae58dd51527b0cf7abcb0a310fb1e7bda78cd2" "8d737627879eff1bbc7e3ef1e9adc657207d9bf74f9abb6e0e53a6541c5f2e88" "5310b88333fc64c0cb34a27f42fa55ce371438a55f02ac7a4b93519d148bd03d" "0eef522d30756a80b28333f05c7eed5721f2ba9b3eaaff244ea4c6f6a1b8ac62" "f67652440b66223b66a4d3e9c0ddeddbf4a6560182fa38693bdc4d940ce43a2e" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" default)))
 '(package-selected-packages
   (quote
    (intero haskell-mode flycheck-cask yatemplate company-erlang ivy-erlang-complete eclipse-theme doom-themes ensime evil firestarter magithub highlight-parenthesis-mode highlight-parenthesis highlight-parentheses dired-k paredit autopair deferred spaceline-config spaceline restclient zone-nyan nyan-mode symon ace-window counsel-projectile counsel swiper powerline git-gutter-fringe arjen-grey-theme multiple-cursors imenu+ avy dracula-theme flycheck-cstyle yaml-mode go-eldoc go-mode beacon drag-stuff gitignore-mode erlang flycheck-dialyxir alchemist flycheck-elixir flycheck-mix neotree flycheck projectile which-key smartparens magit company git-gutter popup-imenu ido-vertical-mode ido-ubiquitous command-log-mode smex highlight-symbol fill-column-indicator solarized-theme rainbow-delimiters exec-path-from-shell all-the-icons use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
