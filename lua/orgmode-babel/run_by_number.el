(defun run-specific-org-babel-block-by-number (block-number)
  "Run a specific org-mode code block, specified by number."
  (save-excursion
    (goto-char (point-min))
    (let ((counter -1))
      (while (re-search-forward "#\\+begin_src" nil t)
        (setq counter (1+ counter))
        (when (= counter block-number)
          (org-babel-execute-src-block))))))

(setq org-confirm-babel-evaluate nil)
(find-file (nth 0 command-line-args-left))

(dolist (block-number (mapcar #'string-to-number (nthcdr 1 command-line-args-left)))
  (run-specific-org-babel-block-by-number block-number))

(save-buffer)
(save-buffers-kill-emacs)

