(defun tangle-org-babel-blocks (&optional block-names)
  "Tangle org-mode code blocks in the current buffer. If BLOCK-NAMES is provided, only tangle those blocks."
  (interactive)
  (if block-names
      (dolist (block block-names)
        (save-restriction
          (goto-char (point-min))
          (while (re-search-forward (concat "#\\+NAME: " (regexp-quote block)) nil t)
            (org-babel-tangle-block))))
    (org-babel-tangle)))

(setq org-confirm-babel-evaluate nil)
(find-file (nth 0 command-line-args-left))

(let ((blocks (cdr command-line-args-left)))
  (if blocks
      (tangle-org-babel-blocks blocks)
    (tangle-org-babel-blocks)))

(save-buffer)
(save-buffers-kill-emacs)

