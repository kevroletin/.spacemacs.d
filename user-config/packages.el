(setq user-config-packages
      '(evil
        helm))

(defun user-config/post-init-evil ()
  (with-eval-after-load 'evil
    ;; Make C-i C-o behave similar with VIM
    (define-key evil-normal-state-map (kbd "C-i") 'evil-jumper/forward)

    ;; Make evil less evil
    (setq evil-cross-lines t)

    ;; C-a already works as in Emacs. Bind C-e for consistency
    (define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)

    ;; Scrolling with C-y is fine by C-y is better
    (global-set-key (kbd "C-y") #'yank)
    (define-key evil-normal-state-map (kbd "C-y") 'evil-paste-after)

    ;; Use tab for indentation like in Emacs
    (global-set-key [tab] #'indent-for-tab-command)))

(defun user-config/post-init-helm ()
  (with-eval-after-load 'helm
    ;; Bind helm functions to Emacs keybinding (for cases when you don't use
    ;; Vim-style)
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)
    (global-set-key (kbd "C-x b") 'helm-mini)
    (global-set-key (kbd "C-c h g") 'helm-do-grep)
    (global-set-key (kbd "C-c h o") 'helm-occur)
    (global-set-key (kbd "C-c h x") 'helm-register)))
