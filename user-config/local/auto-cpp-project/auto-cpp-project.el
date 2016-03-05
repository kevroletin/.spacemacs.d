(require 'dash)
(require 'ede)
(require 'flycheck)
(require 'project-cache)
(require 'projectile)
(require 's)

(defgroup auto-cpp-project nil
  "Automatically configure cpp project for Semantic parser."
  :group 'tools
  :group 'convenience)

(defcustom auto-cpp-system-include-path '()
  "List of directories where Semantic should search cpp
includes."
  :group 'auto-cpp-project
  :type '(repeat string))

(defcustom auto-cpp-project-markers '(".cpp-project")
  "File names which tells that directory contains cpp project"
  :group 'auto-cpp-project
  :type '(repeat string))

(defcustom auto-cpp-project-maybe-markers '()
  "If folder contains one of listed files then auto-cpp-project
will try to guess if this is cpp project"
  :group 'auto-cpp-project
  :type '(repeat string))

(defcustom auto-cpp-project-always-guess '()
  "Always try to guess if directory contains cpp project. Create
\".cpp-project\" marker if no other markers are in directory."
  :group 'auto-cpp-project
  :type '(boolean))

(defcustom auto-cpp-project-name-weights
  '(("inc" . 2)
    ("include" . 2)
    ("lib" . 1)
    ("sdk" . 1))
  "Auto cpp tweaks Semantic to resolve includes choosing best
matching file names. It defines weight function and chooses files
with best (max) score. Weight function counts (multiplying to
weight) how many times listed directories appear in file
path. Auto cpp alwo configures flycheck to look includes inside
directories listed here."
  :group 'auto-cpp-project
  :type '(cons string integer))

(defun auto-cpp--match-cnt (regex string)
  (let ((start 0)
        (res   0))
    (while (string-match regex string start)
      (setq res (+ 1 res))
      (setq start (match-end 0)))
    res))

(defun auto-cpp--weight-function (path)
  (let ((dir (concat "/" (file-name-directory path)))
        (patterns (--map (cons (concat "/" (car it) "/") (cdr it))
                         auto-cpp-project-name-weights)))
    (if (null dir) 0
      (let ((main-const
             (-sum
              (--map
               (* (cdr it) (auto-cpp--match-cnt (car it) dir))
               patterns)))
            (path-len (auto-cpp--match-cnt "/" path)))
        (- (* 100 main-const)
           path-len)))))

(defun auto-cpp--max-index (fun xs)
  (unless (null xs)
    (let ((best-val (funcall fun (car xs)))
          (best-idx 0)
          (rest     (cdr xs))
          (idx      1))
      (while (not (null rest))
        (let ((val (funcall fun (car rest))))
          (when (> val best-val)
            (setq best-val val)
            (setq best-idx idx)))
        (setq rest (cdr rest))
        (setq idx (+ 1 idx)))
      best-idx)))

(defun auto-cpp--choose-with-weight-function (xs)
  (let ((res-idx (auto-cpp--max-index 'auto-cpp--weight-function xs)))
    (nth res-idx xs)))

(defun auto-cpp--is-cpp-file (file)
  (let ((ext (file-name-extension file)))
    (--any (equal ext it) '("h" "hpp" "c" "cpp"))))

(defun auto-cpp--guess-if-cpp-proj (dir)
  (let* ((default-directory dir)
         (all-files (projectile-current-project-files))
         (all-files-cnt (length all-files))
         (cpp-files-cnt (-count 'auto-cpp--is-cpp-file all-files))
         (cpp-percents (/ (* 100 cpp-files-cnt) all-files-cnt)))
    (or (> cpp-files-cnt 100)
        (> cpp-percents  20))))

(defun auto-cpp--is-string-suffix (full-str suffix)
  (or (equal full-str suffix)
      (string-suffix-p (concat "/" suffix) full-str t)))

(defun auto-cpp--locate-file (file-name root-dir)
  (let* ((default-directory root-dir)
         (all-files  (projectile-current-project-files))
         (good-files (--filter (auto-cpp--is-string-suffix it file-name) all-files)))
    (when (not (null good-files))
      (expand-file-name (auto-cpp--choose-with-weight-function good-files)
                        root-dir))))

(defun auto-cpp--project-file (&optional dir)
  (let ((default-directory dir))
    (-when-let (proj (projectile-project-p))
      (let* ((marker-exists (lambda (x) (file-exists-p (expand-file-name x proj))))
             (project-marker
              (or (-find marker-exists auto-cpp-project-markers)
                  (-when-let (marker (-find marker-exists auto-cpp-project-maybe-markers))
                    (and (auto-cpp--guess-if-cpp-proj dir)
                         marker))
                  (and auto-cpp-project-always-guess
                       (auto-cpp--guess-if-cpp-proj dir)
                       (or (-find marker-exists auto-cpp-project-maybe-markers)
                           ".cpp-project")))))
        (when project-marker
          (let ((full-path (expand-file-name project-marker proj)))
            (unless (file-exists-p full-path)
              (with-temp-buffer (write-file full-path)))
            full-path))))))

(defun auto-cpp--project-root (dir)
  (let ((projfile (auto-cpp--project-file (or dir default-directory))))
    (when projfile
      (file-name-directory projfile))))

(defun auto-cpp--load-project (dir)
  (let ((proj-file-name (auto-cpp--project-file dir)))
    (ede-cpp-root-project "some-cool-project"
                          :file proj-file-name
                          :locate-fcn 'auto-cpp--locate-file
                          :system-include-path auto-cpp-system-include-path)))

(defun auto-cpp-register-ede-autoload ()
  (ede-add-project-autoload
   (ede-project-autoload "dynamic-cpp-root"
                         :name "dynamic cpp root"
                         :file 'ede/cpp-root
                         :proj-file 'auto-cpp--project-file
                         :proj-root 'auto-cpp--project-root
                         :load-type 'auto-cpp--load-project
                         :proj-root-dirmatch "*" ;; have no idea what is it
                         :class-sym 'ede-cpp-root-project
                         :new-p nil
                         :safe-p t)
   'unique))

(defun auto-cpp--make-find-cmd ()
  (let* ((xs (--map (concat "-wholename '*/" (car it) "'") auto-cpp-project-name-weights))
         (cmd-part (s-join " -or " xs)))
    (concat "find -type d -and  \\( " cmd-part " \\)")))

(defun auto-cpp--guess-project-includes ()
  (-when-let (default-directory (projectile-project-p))
    (let ((cmd (auto-cpp--make-find-cmd)))
      (--map (expand-file-name it default-directory)
             (split-string (shell-command-to-string cmd) "[\n\t\0]+" t)))))

(defun auto-cpp-configure-flycheck (&optional force)
  (interactive "P")

  (when buffer-file-name
    (let ((dirs (project-cache-get "flycheck-includes")))
      (when (or (null dirs) (consp force))
        (message "Configuring cpp include paths for flycheck")
        (setq dirs (or (auto-cpp--guess-project-includes)
                       "empty"))
        (project-cache-put "flycheck-includes" dirs))
      (unless (equal dirs "empty")
        (make-variable-buffer-local 'flycheck-clang-include-path)
        (make-variable-buffer-local 'flycheck-gcc-include-path)
        (setq flycheck-clang-include-path (-distinct (append flycheck-clang-include-path dirs)))
        (setq flycheck-gcc-include-path   (-distinct (append flycheck-gcc-include-path dirs)))))))

(defun auto-cpp-invalidate-flycheck-configuration-cache ()
  (interactive)
  (project-cache-put "flycheck-includes" '()))

(provide 'auto-cpp-project)
