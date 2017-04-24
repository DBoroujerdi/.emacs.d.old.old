
(defun db/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun db/duplicate-line ()
  "Duplicate the current line and insert the duplicate below the current line"
  (interactive)
  (save-excursion
    (let ((line-contents (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (move-end-of-line 1)
      (newline)
      (insert line-contents))))


(provide 'init-functions)
