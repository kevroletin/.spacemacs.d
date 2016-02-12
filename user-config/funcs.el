(defun user-config/kill-isearch-match ()
  "Kill the current isearch match string and continue searching."
  (interactive)
  (kill-region isearch-other-end (point))
  (isearch-exit))
