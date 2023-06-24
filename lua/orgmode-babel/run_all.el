(defun run-all-org-babel-blocks ()
  "Run all org-mode code blocks in the current buffer."
  (interactive)
  (org-babel-execute-buffer))

(setq org-confirm-babel-evaluate nil)
(find-file (nth 0 command-line-args-left))
(run-all-org-babel-blocks)
(save-buffer)
(save-buffers-kill-emacs)

