;; -*- lexical-binding: t -*-
;;
;; dictionary.com is excellent website to find meaning and pronunciation of
;; English words. If you often find yourself copy-pasting from this site into
;; your text files then this little helper will save you from cumbersome work.

(require 'request-deferred)
(require 's)
(require 'f)

(defun dict-lookup--url-for-word (word)
  (format "http://www.dictionary.com/browse/%s" word))

(defun dict-lookup--find-spelling-in-buffer (buffer)
  (with-current-buffer (current-buffer)
    (switch-to-buffer buffer)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "spellpron" '() t)
        (let* ((a (search-backward "<div>" '() t))
               (b (search-forward  "</div>" '() t))
               (str (buffer-substring-no-properties a b))
               (with-stress (s-replace "<span class=\"dbox-bold\">" "`" str))
               (wo-html (replace-regexp-in-string "</?.*?>" "" with-stress))
               (wo-spaces (-map #'s-trim (s-lines wo-html))))
          (-first-item (-remove #'s-blank? wo-spaces)))))))

(defun dict-lookup--find-spelling-in-current-buffer ()
  (dict-lookup--find-spelling-in-buffer (current-buffer)))

(defun dict-lookup--spelling-at-point-common (callback)
  (-when-let (word (thing-at-point 'symbol))
    (deferred:$
      (request-deferred
       (dict-lookup--url-for-word word)
       :parser 'dict-lookup--find-spelling-in-current-buffer)
      (deferred:nextc it #'request-response-data)
      (deferred:nextc it (lambda (x) (funcall callback word x))))))

(defun dict-lookup-insert-spelling-at-point ()
  (interactive)

  (let ((original-pos (point)))
    (dict-lookup--spelling-at-point-common
     (lambda (word spelling)
       (save-excursion
         (goto-char original-pos)
         (forward-symbol 1)
         (insert " ")
         (insert spelling))))))

(defun dict-lookup-show-spelling-at-point ()
  (interactive)
  (dict-lookup--spelling-at-point-common
   (lambda (w s) (message"%s %s" w s))))

(defun dict-lookup-browser-at-point ()
  (interactive)

  (let ((word (thing-at-point 'symbol)))
    (browse-url (dict-lookup--url-for-word word))))

(provide 'dict-lookup)
