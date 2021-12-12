;;; lisp/org.el -*- lexical-binding: t; -*-

(defun org-heading-is-log-p ()
  "returns non-nil if the heading under the point is a 'Log' heading (ends with the word 'Log')"
  (string-match "^\\*+ .*Log$" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun org-new-log-entry (&optional _)
  "creates a new log entry heading with today's timestamp, creating it's parent 'Log' heading if necessary"

  (interactive)

  ;; Figure out where we are and whether we need to insert a "Log" heading. The end result of each
  ;; conditional should be for the point to be on the "Log" heading line.
  (cond ((org-heading-is-log-p) t)    ; We're on the log heading. Nothing to do.

        ((save-excursion
           (org-backward-heading-same-level 1)
           (org-heading-is-log-p))    ; We're within a log heading so move the point to the heading
         (org-backward-heading-same-level 1))

        ((save-excursion
           (outline-up-heading 1)
           (org-heading-is-log-p))    ; We're on or within a log entry, move to the log heading
         (outline-up-heading 1))

        ;; Looks like we need to create a new log heading
        (t (org-insert-subheading nil)
           (insert "Log")))

  ;; Insert a new log entry at the end of the log
  (org-insert-heading-respect-content)
  (org-demote)
  (org-insert-time-stamp nil nil t)
  (insert "\n")

  ;; Throw in an extra newline below the point
  (save-excursion (insert "\n")))

;;(define-key org-mode-map (kbd "C-c n l") 'org-new-log-entry)
(map! :after evil-org :map evil-org-mode-map
      :n "C-c n l" #'org-new-log-entry)
