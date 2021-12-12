(defun it2-tell-current (&rest messages)
  "Send a message (or list of messages) to the current session of the current window in iTerm2"
  (do-applescript
   (concat
    " tell application \"iTerm2\"\n"
    "   tell the current session of current window\n"
    (string-join messages "\n")
    "\n   end tell\n"
    " end tell\n")))

(defun it2-write-text (text)
  "Write a string into the current session of the current window in iTerm2"
  (it2-tell-current
   (concat "write text \""
           (replace-regexp-in-string "\\([\"\\]\\)" "\\\\\\1" text)
           "\"")))

(defun it2-line-continued-p (&optional n)
  "returns non-nil if the line ends with a backslash"
  (string= "\\" (let (p)
                  (setq p (line-end-position n))
                  (buffer-substring (- p 1) p))))

(defun it2-current-continued-line ()
  "returns the current line including lines which may have been continued with backslashes"
  (save-excursion
    ;; Move up to the first line of this continuation
    (while (it2-line-continued-p 0)
      (forward-line -1))

    ;; Save the point and move forward to to the last line of the continuation
    (let ((s (start-of-line-at-pos (point))))
      (while (it2-line-continued-p)
        (forward-line))
      (substring-no-properties (buffer-substring s (end-of-line-at-pos (point)))))))

(defun it2-write-region-or-line (s e)
  "send the content of the current region if there is one or line if not"
  (interactive "r")
  (it2-write-text
   (if (and mark-active (/= (point) (mark)))      
       (buffer-substring s e) ;; send the selected region
     (it2-current-continued-line))))

;;(global-set-key (kbd "s-<return>") 'it2-write-region-or-line)
(map! "M-<return>" #'it2-write-region-or-line)
