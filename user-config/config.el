;; Exclude service directories and binary files from grep results
;; and from project files list [1]
(defvar user-config-ignored-directories '(".svn" ".git"))

(defvar user-config-ignored-files
  '("GTAGS" "GRAPH" "GRTAGS" "*.chm" "*.exe" "*.obj" "*.o" "*.class"))

;; [1] http://tuhdo.github.io/helm-projectile.html
