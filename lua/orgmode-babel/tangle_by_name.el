(defun tangle-block-by-name (block-name)
  "Tangle the org-babel code block with name BLOCK-NAME."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (format "#\\+NAME: %s" block-name) nil t)
      (if (org-babel-get-src-block-info)
          (org-babel-tangle-collect-block)))))


(setq org-confirm-babel-evaluate nil)
(find-file (nth 0 command-line-args-left))

(dolist (block-name (nthcdr 1 command-line-args-left))
  (tangle-block-by-name block-name))

(save-buffer)
(save-buffers-kill-emacs)

