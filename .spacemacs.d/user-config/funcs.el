(defun user-config/kill-isearch-match ()
  "Kill the current isearch match string and continue searching."
  (interactive)
  (kill-region isearch-other-end (point))
  (isearch-exit))

(defun user-config/add-all-to-list (to values)
  (dolist (x values)
    (add-to-list to x)))

(defun user-config//load-literate-config (name)
  (let ((dir (expand-file-name ".spacemacs.d/user-config/literate-configs" (getenv "HOME"))))
    (org-babel-load-file (expand-file-name name dir))))
