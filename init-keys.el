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
(global-set-key (kbd "C-/") 'comment-line)

;; a more concise way of killing the current buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; duplicate line
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y")

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-u") 'undo)

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line] 'db/smarter-move-beginning-of-line)

(provide 'init-keys)
