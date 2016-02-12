;; Switch "delete all spaces" and "delete all but one spaces" because first
;; one is more useful
(global-set-key (kbd "M-SPC") #'delete-horizontal-space)
(global-set-key (kbd "M-\\") #'just-one-space)

;; Helpers to quickly cycle  buffers
(global-set-key (kbd "C-<") #'previous-buffer)
(global-set-key (kbd "C->") #'next-buffer)

;; Spacemacs binds help to <space>-h-d or <f1> which means C-h could be used for
;; something else. Let's use it to delete char backwards since C-h is standard
;; backward delete shortcut both in many UNIX tools and in VIM
(bind-key "C-h" #'backward-delete-char-untabify)

;; This trick is useful when you changed your mind during search and decided to
;; do replace in single place
(define-key isearch-mode-map [(control k)] 'user-config/kill-isearch-match)
