(setq user-config-packages
      '(evil
        helm
        ;; Transitive dependencies
        dash
        ede
        flycheck
        projectile
        s
        semantic
        ;; Local code
        (project-cache       :location local)
        (compile-per-project :location local)
        (auto-cpp-project    :location local)))

(defun user-config/post-init-evil ()
  (with-eval-after-load 'evil
    ;; Make C-i C-o behave similar with VIM
    (define-key evil-normal-state-map (kbd "C-i") 'evil-jumper/forward)
    ;; Make evil less evil
    (setq evil-cross-lines t)
    ;; C-a already works as in Emacs. Bind C-e for consistency
    (define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
    ;; Scrolling with C-y is fine by C-y for yank is better
    (global-set-key (kbd "C-y") #'yank)
    (define-key evil-normal-state-map (kbd "C-y") 'evil-paste-after)
    ;; Use tab for indentation like in Emacs
    (global-set-key [tab] #'indent-for-tab-command)))

(defun user-config/post-init-helm ()
  (with-eval-after-load 'helm
    (setq projectile-require-project-root t)
    (setq projectile-enable-caching t)
    ;; Bind helm functions to Emacs keybinding (for cases when you don't use
    ;; Vim-style)
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)
    (global-set-key (kbd "C-x b") 'helm-mini)
    (global-set-key (kbd "C-c h g") 'helm-do-grep)
    (global-set-key (kbd "C-c h o") 'helm-occur)
    (global-set-key (kbd "C-c h x") 'helm-register)))

(defun user-config/init-project-cache ()
  (use-package project-cache
    :commands (project-cache-file
               project-cache-put
               project-cache-get)
    :config
    (defadvice projectile-invalidate-cache
        (before sk-invalidate-flycheck-settings-cache (args) activate)
      (interactive "P")
      (auto-cpp-invalidate-flycheck-configuration-cache))))

(defun user-config/init-compile-per-project ()
  (use-package compile-per-project
    :bind
    ("<f5>" . compile-per-project-compile)
    ("<f6>" . compile-per-project-run)
    ("<f7>" . compile-per-project-test)))

(defun user-config/init-auto-cpp-project ()
  (use-package auto-cpp-project
    :commands (auto-cpp-configure-flycheck
               auto-cpp-register-ede-autoload)
    :init
    (progn
      (spacemacs/add-to-hooks 'auto-cpp-configure-flycheck
                              '(c-mode-hook c++-mode-hook)))))

(defun user-config/post-init-semantic ()
  (with-eval-after-load 'semantic
    ;; Without EDE next auto-cpp-project doesn't work. According to
    ;; documentation "EDE is implemented as a global minor mode"
    (global-ede-mode 1)
    (auto-cpp-register-ede-autoload)

    ;; It appearts that global minor modes override default minor mode settings
    ;; configured by Semantic layer
    (global-semantic-show-parser-state-mode 1)
    (global-semantic-show-unmatched-syntax-mode 1)
    (global-semantic-decoration-mode 1)
    (global-semantic-idle-summary-mode -1)

    ;; TODO: move into color theme customization
    (custom-set-faces
     '(semantic-unmatched-syntax-face ((t (:background "grey20"))))
     '(semantic-tag-boundary-face ((t nil)))
     '(semantic-decoration-on-unknown-includes ((t (:inherit flyspell-incorrect)))))))
