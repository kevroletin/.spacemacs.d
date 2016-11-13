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

(defun user-config/get-jira-secret ()
  (let* ((auth (nth 0 (auth-source-search :host "jira.rhonda.ru"
                                          :requires '(user secret))))
         (pass (funcall (plist-get auth :secret)))
         (user (plist-get auth :user)))
    (base64-encode-string (concat user ":" pass))))

(defun user-config/org-expand-all-tables ()
  (interactive)

  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[|-\+]+$" '() t)
      (org-ctrl-c-ctrl-c))))
