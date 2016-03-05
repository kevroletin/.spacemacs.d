(setq user-theme-packages
      '((atom-dark-tuned-theme :location local)))

(defun user-theme/init-atom-dark-tuned-theme ()
  (let* ((current-dir (file-name-directory (or load-file-name buffer-file-name)))
         (theme-dir (expand-file-name "local/atom-dark-tuned-theme" current-dir)))
    (add-to-list 'custom-theme-load-path (file-name-as-directory theme-dir)))
  (require 'atom-dark-tuned-theme)
  (load-theme 'atom-dark-tuned t))
