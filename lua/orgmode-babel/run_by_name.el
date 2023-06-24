(defun run-specific-org-babel-block (block-name)
  "Run a specific org-mode code block, specified by name."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (search-forward (concat "#+NAME: " block-name) nil t)
        (org-babel-execute-src-block)))))

(setq org-confirm-babel-evaluate nil)
(find-file (nth 0 command-line-args-left))

(dolist (block-name (nthcdr 1 command-line-args-left))
  (run-specific-org-babel-block block-name))

(save-buffer)
(save-buffers-kill-emacs)

