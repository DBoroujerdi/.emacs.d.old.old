;; window tabbing
(global-set-key [C-tab] 'other-window)
(global-set-key [C-S-iso-lefttab] 'previous-multiframe-window)

(global-set-key (kbd "C-x c") 'comment-region)
(global-set-key (kbd "C-x x") 'uncomment-region)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)

;; window tabbing
(global-set-key [C-tab] 'other-window)
(global-set-key [C-S-iso-lefttab] 'previous-multiframe-window)

(global-set-key (kbd "M-d") 'db/duplicate-line)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-/") 'comment-line)

;; magit shortcuts
(global-set-key (kbd "C-c s") 'magit-status)
(global-set-key (kbd "C-c l") 'magit-log)
(global-set-key (kbd "C-c c") 'magit-checkout)
(global-set-key (kbd "C-c b") 'magit-blame)

;; move buffer with Shift
(global-set-key (kbd "S-<left>")  'windmove-left)
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<up>")    'windmove-up)
(global-set-key (kbd "S-<down>")  'windmove-down)

(global-set-key (kbd "C-z") 'imenu)

;; duplicate line
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y")

(global-set-key (kbd "M-/") 'hippie-expand)

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line] 'db/smarter-move-beginning-of-line)

(provide 'init-keys)
