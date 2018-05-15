(setq user-config-packages
      '((jiraffe :location (recipe :fetcher github :repo "kevroletin/jiraffe.el"))
        ansi-color
        auth-source
        cc-mode
        evil
        grep
        helm
        org
        projectile
        ;; Transitive dependencies
        (lifted :location (recipe :fetcher github :repo "inlinestyle/lifted.el"))
        dash
        deferred
        ede
        flycheck
        htmlize
        request-deferred
        s
        semantic
        ;; Local code
        (auto-cpp-project    :location local)
        (compile-per-project :location local)
        (dict-lookup         :location local)
        (ghcid               :location local)
        (project-cache       :location local)))

(defun user-config/init-user-config ()
  (spacemacs/set-leader-keys "os" #'user-config/sh-send-code-block))

(defun user-config/init-ghcid ()
  (use-package ghcid
    :commands (ghcid
               ghcid-stop)))

(defun user-config/init-dict-lookup ()
  (use-package dict-lookup
    :commands (dict-lookup-insert-spelling-at-point
               dict-lookup-browser-at-point
               dict-lookup-show-spelling-at-point))
  (spacemacs/declare-prefix "ad" "dict-lookup")
  (spacemacs/set-leader-keys "odi" #'dict-lookup-insert-spelling-at-point)
  (spacemacs/set-leader-keys "odo" #'dict-lookup-browser-at-point)
  (spacemacs/set-leader-keys "ods" #'dict-lookup-show-spelling-at-point))

(defun user-config/init-lifted ())

(defun user-config/init-deferred ())

(defun user-config/init-request-deferred ())

(defun user-config/init-jiraffe ()
  (spacemacs/declare-prefix "aj" "jira")
  (spacemacs/set-leader-keys "ojF" #'jiraffe-insert-filter-result-here)
  (spacemacs/set-leader-keys "ojf" #'jiraffe-show-filter-result)
  (spacemacs/set-leader-keys "ojy" #'jiraffe-yank-issue))

(defun user-config/post-init-cc-mode ()
  (with-eval-after-load 'cc-mode
    (setq c-default-style "bsd")
    (setq c-basic-offset 4)))

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

(defun user-config/post-init-grep ()
  (with-eval-after-load 'grep
    (user-config/add-all-to-list 'grep-find-ignored-files       user-config-ignored-files)
    (user-config/add-all-to-list 'grep-find-ignored-directories user-config-ignored-directories)))

(defun user-config/post-init-helm ()
  (with-eval-after-load 'helm
    ;; Bind helm functions to Emacs keybinding (for cases when you don't use
    ;; Vim-style or Emacs-style)
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)
    (global-set-key (kbd "C-x b") 'helm-mini)
    (global-set-key (kbd "C-c h g") 'helm-do-grep)
    (global-set-key (kbd "C-c h o") 'helm-occur)
    (global-set-key (kbd "C-c h x") 'helm-register)))

(defun user-config/post-init-org ()
  (with-eval-after-load 'org
    (user-config//load-literate-config "org-mode.org")))

(defun user-config/post-init-projectile ()
  (with-eval-after-load 'projectile
    (setq projectile-require-project-root t)
    (setq projectile-enable-caching t)

    ;; Treat .svn directory as project root. The topmost .svn folder has
    ;; precedence because svn externals place .svn directories inside other svn
    ;; repository.
    (add-to-list 'projectile-project-root-files ".svn")

    (user-config/add-all-to-list 'projectile-globally-ignored-files       user-config-ignored-files)
    (user-config/add-all-to-list 'projectile-globally-ignored-directories user-config-ignored-directories)))

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
               auto-cpp-register-ede-autoload
               auto-cpp-invalidate-flycheck-configuration-cache)
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
     '(semantic-decoration-on-unknown-includes ((t (:inherit flyspell-incorrect)))))

    (spacemacs/set-leader-keys "oj" 'semantic-ia-fast-jump)))

(defun user-config/init-ansi-color ()
  (with-eval-after-load 'ansi-color
    (defun user-config//colorize-compilation-buffer ()
      (when (eq major-mode 'compilation-mode)
        (ansi-color-apply-on-region compilation-filter-start (point-max))))
    (add-hook 'compilation-filter-hook 'user-config//colorize-compilation-buffer)))

;; [2] http://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
