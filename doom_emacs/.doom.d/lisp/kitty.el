;;; ../dotfiles/doom_emacs/.doom.d/lisp/kitty.el -*- lexical-binding: t; -*-

(defvar kitty-socket-glob "~/.kitty.sock-*"
  "This value will be passed to file-expand-wildcards to obtain a
list of active kitty terminal sockets.")

(defvar-local kitty-socket-current nil
  "The path to the kitty terminal Unix socket that commands will be sent to.")

(defun kitty-packet (data)

  "Returns a string which can be passed to a kitty socket. DATA
should be a structure like you would pass to json-encode."

  (concat "\eP@kitty-cmd" (json-encode data) "\e\\\n"))

(defun send-packet-to-kitty (socket-path data)

  "Sends the message in DATA to the kitty terminal listening on
Unix socket SOCKET-PATH. DATA should be a structure like you
would pass to json-encode."

  (let ((sock (make-network-process :name "*kitty*"
                                    :family 'local
                                    :service socket-path)))
    (unwind-protect
        (process-send-string sock (kitty-packet data))
      (delete-process sock))))

(defun send-to-kitty (socket-path cmd payload &optional want-response)

  "Send command CMD to kitty terminal listening on Unix socket
SOCKET-PATH with PAYLOAD. If a reply is expected set
WANT-RESPONSE to t."

  (send-packet-to-kitty socket-path `((cmd . ,cmd)
                                      (version . (0 14 2))
                                      (no_response . ,(not want-response))
                                      (payload . ,payload))))

(defun send-text-to-kitty (socket-path text)

  "Sends the string TEXT to the kitty terminal listening on Unix
socket SOCKET-PATH."

  (send-to-kitty socket-path "send-text" `((data . ,(concat "text:" text)))))

(defun set-kitty-window-title (socket-path title &optional temporary)

  "Sets the window title of the kitty terminal listening on Unix
socket SOCKET-PATH to TITLE. If TEMPORARY is t then the title
will be reset on the next command within the window."

  (send-to-kitty socket-path "set-window-title" `((title . ,title)
                                                  (temporary . ,temporary))))

(defun all-kitty-sockets ()

  "Returns a list of open kitty Unix sockets by evaluating
  kitty-socket-glob."

  (mapcar 'file-truename (file-expand-wildcards kitty-socket-glob)))

(defun label-kitty-windows-for-selection (sockets)

  "Labels kitty terminal windows listening on SOCKETS with their
index idex in the list starting at 0."

  (let ((i 0))
    (while sockets
      (set-kitty-window-title (first sockets) (format "Select %d in Emacs" i) t)
      (setq i (1+ i))
      (setq sockets (cdr sockets)))))

(defun select-kitty-terminal ()

  "Returns the path to the Unix socket for the user-selected
kitty terminal. If there are no available sockets, returns nil.
If there is only one available, that socket is automatically
selected. Otherwise the user is prompted to select the desired
terminal."

  (let ((sockets (all-kitty-sockets)))
    (pcase (length sockets)
      (0 (message "No kitty sockets found: %s" kitty-socket-glob))
      (1 (first sockets))
      (_ (label-kitty-windows-for-selection sockets)
         (nth (read-number "Enter kitty term number: ") sockets)))))

(defun get-current-kitty-terminal ()

  "Returns the Unix socket for the current kitty terminal
associated with this buffer based on the value in
kitty-socket-current. If this value is nil or points to a
non-existant path, then a new selection is made, prompting the
user if necessary."

  (if (and kitty-socket-current (file-exists-p kitty-socket-current))
      kitty-socket-current
    (when-let ((sel (select-kitty-terminal)))
      (setq-local kitty-socket-current sel)
      sel)))

(defun kitty-line-continued-p (&optional n)

  "Returns non-nil if the line ends with a backslash."

  (string= "\\" (let (p)
                  (setq p (line-end-position n))
                  (buffer-substring (- p 1) p))))

(defun kitty-current-continued-line ()

  "Returns the current line including lines which may have been
continued with backslashes."

  (save-excursion
    ;; Move up to the first line of this continuation
    (while (kitty-line-continued-p 0)
      (forward-line -1))

    ;; Save the point and move forward to to the last line of the continuation
    (let ((s (start-of-line-at-pos (point))))
      (while (kitty-line-continued-p)
        (forward-line))
      (substring-no-properties (buffer-substring s (end-of-line-at-pos (point)))))))

(defun kitty-write-region-or-line (s e)

  "Send the content of the current region if there is one or line if not."

  (interactive "r")
  (send-text-to-kitty
   (get-current-kitty-terminal)
   (concat
    (if (and mark-active (/= (point) (mark)))
        (buffer-substring s e) ;; send the selected region
      (kitty-current-continued-line))
    "\n")))

;;(global-set-key (kbd "s-<return>") 'kitty-write-region-or-line)
(map! "M-<return>" #'kitty-write-region-or-line)
