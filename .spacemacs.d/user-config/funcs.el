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

(defun user-config//get-code-block ()
  (require 'org)

  (if (use-region-p)
      (concat (buffer-substring-no-properties (region-beginning)
                                              (region-end)) "\n")
    (if (org-in-block-p '("src" "example"))
        (org-element-property :value (org-element-at-point))
      (concat (buffer-substring-no-properties (point-at-bol)
                                              (point-at-eol)) "\n"))))

;; Modified version of https://stackoverflow.com/a/7053298/3168464
(defun user-config/sh-send-code-block ()
  "Sends 'code block' under cursor into shell buffer. If there is
no shell buffer then it will spawn one. 'code block' is either
selected region or org-mode code block (src or example) or
current line"
  (interactive ())
  (let ((proc (get-process "shell"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (shell)
        (switch-to-buffer currbuff)
        (setq proc (get-process "shell"))))
    (setq pbuff (process-buffer proc))
    (setq command (user-config//get-code-block))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command)
      (move-marker (process-mark proc) (point))
      ) ;;pop-to-buffer does not work with save-current-buffer -- bug?
    (process-send-string  proc command)
    (display-buffer (process-buffer proc) t)))
