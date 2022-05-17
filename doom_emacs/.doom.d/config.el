;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(defmacro comment (&rest a))


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Dan Boitnott"
      user-mail-address "dan@lclinux.org")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;;
;; Fira Code font installation:
;;   https://github.com/tonsky/FiraCode/wiki/Installing
(setq doom-font (font-spec :family "Fira Code" :weight 'semilight :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; `doom-one' is the default
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(setq fancy-splash-image (concat doom-private-dir "splash/meditate-290.png"))

(defun doom-dashboard-draw-ascii-banner-fn ()
  (let* ((banner
          '("  ,           ,"
            " /             \\"
            "((__-^^-,-^^-__))"
            " `-_---' `---_-'"
            "  `--|o` 'o|--'"
            "     \\  `  /"
            "      ): :("
            "      :o_o:"
            "       \"-\""))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(load! "lisp/editing.el")

;; TODO: Detect when we're on macos and use iterm2 integration
                                        ;(load! "lisp/iterm2-integration.el")
(load! "lisp/kitty.el")

(load! "lisp/org.el")

;; Company mode causes hangs in sh-mode so we disable it.
(defun disable-company ()
  (interactive)
  (company-mode 0))
(add-hook! sh-mode #'disable-company)

(use-package! python-black
  :demand t
  :after python
  :config
  (add-hook! 'python-mode-hook #'python-black-on-save-mode)
  ;; Feel free to throw your own personal keybindings here
  ;; (map! :leader :desc "Blacken Buffer" "m b b" #'python-black-buffer)
  ;; (map! :leader :desc "Blacken Region" "m b r" #'python-black-region)
  ;; (map! :leader :desc "Blacken Statement" "m b s" #'python-black-statement)
  )

;; (use-package! lsp-python-ms
;;    :init (setq lsp-python-ms-parse-dot-env-enabled t))

(setq lsp-disabled-clients '(mspyls))

;; I'm ignoring "line too long" errors because I'm using the Python Black
;; auto-formatter and it sometimes allows lines to be slightly long.
(setq lsp-pylsp-plugins-flake8-ignore ["E501" "W503"])

;; (use-package! zprint-mode
;;   :config
;;   (add-hook! 'clojure-mode-hook #'zprint-mode)
;;   (add-hook! 'clojurescript-mode-hook #'zprint-mode))

(add-hook! 'clojure-mode-hook #'format-all-mode)
(add-hook! 'clojurescript-mode-hook #'format-all-mode)
(add-hook! 'clojure-mode-hook #'paredit-mode)
(add-hook! 'clojurescript-mode-hook #'paredit-mode)

;; Custom keybindings
(map! "<M-/>" #'comment-line)

;; Insert mail signatures above quoted text
(defun message-insert-signature-at-point (pmode)
  "Function to insert signature at point."
  (interactive)
  (when pmode (mu4e-compose-goto-top))
                                        ;(require 'message)
  (message-goto-body)
  (save-restriction
    (narrow-to-region (point) (point))
    (message-insert-signature))
  (mu4e-compose-goto-top))

(add-hook 'mu4e-compose-mode-hook (lambda () (message-insert-signature-at-point nil)) t)
(add-hook 'mu4e-compose-pre-hook (lambda () (message-insert-signature-at-point t)) t)

(after! mu4e
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext")
  (add-to-list 'mu4e-bookmarks '(:name "INBOX" :query "m:/sig/Inbox OR m:/gmail/Inbox" :key ?i))
  (setq
   ;; set correct path
   sendmail-program "/usr/bin/msmtp"
   mu4e-root-maildir "~/.mail"
   mu4e-mu4e-mail-path "~/.mail"
   send-mail-function  'smtpmail-send-it
   ;; remove adding username --> msmtp takes care of this
   message-sendmail-f-is-evil t
   ;; read who is sending the email
   message-sendmail-extra-arguments '("--read-envelope-from")
   message-send-mail-function 'message-send-mail-with-sendmail
   mu4e-get-mail-command "mbsync sig:INBOX gmail:INBOX"

   mu4e-headers-include-related nil

   ;; Disable the default signature behavior so that it can be added above
   ;; quoted text by a hook (defined above).
   mu4e-compose-signature-auto-include nil

   mu4e-contexts `(,(make-mu4e-context
                     :name  "sig"
                     :enter-func (lambda () (mu4e-message "Entering SIG context."))
                     :match-func (lambda (msg)
                                   (when msg
                                     (string-prefix-p "/sig" (mu4e-message-field msg :maildir))))

                     :vars '((mu4e-sent-folder . "/sig/Sent Items")
                             (mu4e-drafts-folder . "/sig/Drafts")
                             (mu4e-inbox-folder . "/sig/Inbox")
                             (mu4e-trash-folder . "/sig/Trash")
                             (mu4e-refile-folder . "/sig/Archive")
                             (mu4e-compose-signature . "Dan Boitnott
Senior DBA, Cloud Architect
Strata Information Group
boitnott@sigcorp.com
337-240-6326")
                             (smtpmail-local-domain . "sig")
                             (smtpmail-smtp-server . "smtp.office365.com")
                             (smtpmail-default-smtp-server . "smtp.office365.com")
                             (smtpmail-smtp-service . 587)
                             (user-mail-address . "boitnott@sigcorp.com")))

                   ,(make-mu4e-context
                     :name  "gmail"
                     :enter-func (lambda () (mu4e-message "Entering gmail context."))
                     :match-func (lambda (msg)
                                   (when msg
                                     (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))

                     :vars '((mu4e-sent-folder . "/gmail/[Gmail]/Sent Mail")
                             (mu4e-drafts-folder . "/gmail/[Gmail]/Drafts")
                             (mu4e-inbox-folder . "/gmail/Inbox")
                             (mu4e-trash-folder . "/gmail/[Gmail]/Trash")
                             (mu4e-refile-folder . "/gmail/Archive")
                             (user-mail-address . "dboitnot@gmail.com")
                             (mu4e-compose-signature . "")

                             (smtpmail-local-domain . "gmail")
                             (smtpmail-smtp-server . "smtp.gmail.com")
                             (smtpmail-default-smtp-server . "smtp.gmail.com")
                             (smtpmail-smtp-service . 1025))))))

(setq elfeed-feeds '(("https://lastweekinaws.com/feed" aws)))

;;
;; org-mode config
;;

(require 'org-habit)
(require 'org-super-agenda)
(org-super-agenda-mode)
(add-to-list 'org-modules 'org-habit)

(after! org
  ;; Remove the Doom-delivered todo template and replace it with a slightly different one.
  (setq org-capture-templates
        (seq-filter (lambda (i) (not (s-equals? "t"  (nth 0 i)))) org-capture-templates))
  (add-to-list 'org-capture-templates
               '("t" "Personal todo" entry
                 (file+headline +org-capture-todo-file "Inbox")
                 "* TODO %?\n%i\n%a" :prepend t)))

(setq org-startup-folded t
      ;; Compact the block agenda view
      org-agenda-compact-blocks t
      org-agenda-start-day "0d"
      org-super-agenda-mode t

      org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                          (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))

      org-todo-keyword-faces  '(("TODO" :foreground "forest green" :weight bold)
                                ("NEXT" :foreground "royal blue" :weight bold)
                                ("DONE" :foreground "grey54" :weight bold)
                                ("WAIT" :foreground "yellow" :weight bold)
                                ("HOLD" :foreground "thistle4" :weight bold)
                                ("CANCELLED" :foreground "grey54" :weight bold)
                                ("MEETING" :foreground "forest green" :weight bold)
                                ("PHONE" :foreground "forest green" :weight bold))

      ;; Custom agenda command definitions
      org-agenda-custom-commands
      '(("A" "Agenda"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                          :time-grid t
                          :date today
                          :todo "TODAY"
                          :scheduled today
                          :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '(;; Each group has an implicit boolean OR operator between its selectors.
                          (:name "Today"
                           :deadline today
                           :face (:background "black"))
                          (:name "Passed deadline"
                           :and (:deadline past :todo ("TODO" "WAIT" "HOLD" "NEXT"))
                           :face (:background "#7f1b19"))
                          (:name "SIG important"
                           :and (:priority>= "B" :tag "SIG" :todo ("TODO" "NEXT")))
                          (:name "Inbox"
                           :and (:tag "inbox"))
                          (:name "SIG Next"
                           :and (:tag "SIG" :todo ("NEXT")))
                          (:name "SIG other"
                           :and (:tag "SIG" :todo ("TODO")))
                          (:name "Important"
                           :priority "A")
                          (:priority<= "B"
                           ;; Show this section after "Today" and "Important", because
                           ;; their order is unspecified, defaulting to 0. Sections
                           ;; are displayed lowest-number-first.
                           :order 1)
                          (:name "Papers"
                           :file-path "org/roam/notes")
                          (:name "Waiting"
                           :todo "WAIT"
                           :order 9)
                          (:name "On hold"
                           :todo "HOLD"
                           :order 10)))))))))
