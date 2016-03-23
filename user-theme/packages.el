(setq user-theme-packages
      '((atom-dark-tuned-theme :location local)
        (base16-theme)))

(defun user-theme/init-atom-dark-tuned-theme ()
  (when user-theme-replace-atom-dark
    (let* ((current-dir (file-name-directory (or load-file-name buffer-file-name)))
           (theme-dir (expand-file-name "local/atom-dark-tuned-theme" current-dir)))
      (add-to-list 'custom-theme-load-path (file-name-as-directory theme-dir)))
    (require 'atom-dark-tuned-theme)
    (load-theme 'atom-dark-tuned t)))

;; Spacemacs doesn't define init functions for color themes. So it's safe to
;; define it ourselves.
;;
;; fix for company tooltip with dark theme is from https://www.emacswiki.org/emacs/CompanyMode
(defun user-theme/init-base16-theme()
  ;; I am too lazy to define init function for color
  (require 'color)

  ;; TODO: find out if it possible to get colors from theme instead of
  ;; copy-paste
  (let* ((base00 "#181818")
         (base01 "#282828")
         (base02 "#383838")
         (base03 "#585858")
         (base04 "#b8b8b8")
         (base05 "#d8d8d8")
         (base06 "#e8e8e8")
         (base07 "#f8f8f8")
         (base08 "#ab4642")
         (base09 "#dc9656")
         (base0A "#f7ca88")
         (base0B "#a1b56c")
         (base0C "#86c1b9")
         (base0D "#7cafc2")
         (base0E "#ba8baf")
         (base0F "#a16946")

         (blue base0C)
         (bg base00)
         (fg base05))

    (custom-theme-set-faces
     'base16-default-dark

     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))
     `(company-preview ((t (:foreground ,blue))))
     `(company-preview-common ((t (:inherit company-preview :underline ,blue))))
     `(company-preview-search ((t (:inherit company-preview))))

     `(fringe ((t (:background ,bg :foreground ,fg))))
     `(sp-show-pair-match-face ((t (:foreground "green"))))

     `(linum ((t (:background ,bg :foreground ,base03))))
     `(linum-relative-current-face ((t (:inherit linum)))))))
