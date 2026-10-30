;;; ghostel-terminal-list.el --- Every ghostel terminal and its program status -*- lexical-binding: t; -*-

;; Author: Daniel Kraus <daniel@kraus.my>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; One view over every ghostel terminal and its program status records
;; (OSC 7501, OSC 9;4), for when many programs run in terminals you are
;; not looking at.  `ghostel-terminal-list' lists every terminal with
;; its records; `ghostel-status-next' switches to the terminals that need
;; you and `ghostel-status-indicator-mode' counts terminals by state in
;; every mode line.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ghostel)
(require 'ghostel-project)
(require 'ghostel-status)


;;; Customization

(defcustom ghostel-terminal-list-attention-only nil
  "Non-nil opens `ghostel-terminal-list' showing only terminals that need you.
Those are the terminals with a record in `ghostel-status-notify-states';
nil shows every ghostel terminal.  `ghostel-terminal-list-toggle-attention-only'
switches between the two in the terminal list."
  :type 'boolean
  :group 'ghostel)

(defcustom ghostel-terminal-list-full-directory nil
  "Non-nil opens `ghostel-terminal-list' showing each whole directory.
A terminal's whole directory then shows on its own line below its
records; nil shows the last component of the directory in the heading.
`ghostel-terminal-list-toggle-full-directory' switches between the two
in the terminal list."
  :type 'boolean
  :group 'ghostel)

(defcustom ghostel-terminal-list-name-width 16
  "Columns `ghostel-terminal-list' pads a terminal's name to.
Titles after names up to this wide line up."
  :type 'natural
  :group 'ghostel)

(defcustom ghostel-terminal-list-symbols
  '((blocked . "✋") (error . "❌") (done . "✅") (working . "⏳"))
  "Symbol per program status state, leading each record in the terminal list.
A state without an entry shows its name only.  A blocked record's kind
can have its own symbol in `ghostel-terminal-list-blocked-symbols'."
  :type '(alist :key-type symbol :value-type string)
  :group 'ghostel)

(defcustom ghostel-terminal-list-blocked-symbols
  '((permission . "🔒") (auth . "🔑") (question . "💬"))
  "Symbol per kind of blocked record, in place of the `blocked' symbol."
  :type '(alist :key-type symbol :value-type string)
  :group 'ghostel)

(defface ghostel-terminal-list-directory
  '((((class color) (min-colors 88) (background dark)) :foreground "#6c7a96")
    (((class color) (min-colors 88) (background light)) :foreground "#8090b0")
    (t :inherit shadow))
  "Face for a terminal's directory in the terminal list.
Dimmer than `shadow', with a blue tint."
  :group 'ghostel)


;;; Records across terminals

(defconst ghostel-terminal-list-buffer-name "*ghostel-terminals*"
  "Name of the `ghostel-terminal-list' buffer.")

(defvar ghostel-terminal-list--since (make-hash-table :test #'equal)
  "Time each record entered its state, keyed by (BUFFER . ID).")

(defvar ghostel-terminal-list--timer nil
  "Pending refresh timer, or nil.")

(defun ghostel-terminal-list--track (id old new)
  "Note when record ID changes state from OLD to NEW and schedule a refresh.
A member of `ghostel-program-status-functions'."
  (let ((key (cons (current-buffer) id)))
    (cond ((null new) (remhash key ghostel-terminal-list--since))
          ((not (eq (plist-get old :state) (plist-get new :state)))
           (puthash key (float-time) ghostel-terminal-list--since))))
  (ghostel-terminal-list--schedule-refresh))

(defun ghostel-terminal-list--start ()
  "Start tracking status changes and terminals opening and closing."
  (add-hook 'ghostel-program-status-functions #'ghostel-terminal-list--track 90)
  (add-hook 'ghostel-mode-hook #'ghostel-terminal-list--schedule-refresh)
  (add-hook 'kill-buffer-hook #'ghostel-terminal-list--on-kill))

(defun ghostel-terminal-list--rank (entry)
  "Return the urgency rank of ENTRY's state, lower is more urgent."
  (or (cl-position (plist-get (nth 2 entry) :state) ghostel-status--urgency)
      (length ghostel-status--urgency)))

(defun ghostel-terminal-list--entries (&optional states)
  "Return (BUFFER ID RECORD SINCE) for every record, most urgent first.
Only records in STATES when non-nil.  SINCE is the `float-time' the
record entered its state, nil when not known."
  (maphash (lambda (key _)
             (unless (buffer-live-p (car key))
               (remhash key ghostel-terminal-list--since)))
           ghostel-terminal-list--since)
  (let (entries)
    (dolist (buf (ghostel-buffer-list))
      (dolist (entry (buffer-local-value 'ghostel-program-status buf))
        (when (or (null states) (memq (plist-get (cdr entry) :state) states))
          (push (list buf (car entry) (cdr entry)
                      (gethash (cons buf (car entry)) ghostel-terminal-list--since))
                entries))))
    (sort (nreverse entries)
          (lambda (a b)
            (let ((ra (ghostel-terminal-list--rank a))
                  (rb (ghostel-terminal-list--rank b)))
              (if (/= ra rb)
                  (< ra rb)
                (< (or (nth 3 a) 0) (or (nth 3 b) 0))))))))

(defun ghostel-terminal-list--schedule-refresh ()
  "Refresh the terminal list buffer soon, once per burst."
  (unless ghostel-terminal-list--timer
    (setq ghostel-terminal-list--timer
          (run-at-time 0.1 nil #'ghostel-terminal-list--refresh))))

(defun ghostel-terminal-list--refresh ()
  "Refresh the terminal list buffer."
  (setq ghostel-terminal-list--timer nil)
  (when-let* ((buf (get-buffer ghostel-terminal-list-buffer-name)))
    (with-current-buffer buf
      (when (derived-mode-p 'ghostel-terminal-list-mode)
        (ghostel-terminal-list--render)))))

(defun ghostel-terminal-list--on-kill ()
  "Refresh the terminal list when a terminal is killed."
  (when (and (derived-mode-p 'ghostel-mode)
             (get-buffer ghostel-terminal-list-buffer-name))
    (ghostel-terminal-list--schedule-refresh)))


;;; Terminal list buffer

(defun ghostel-terminal-list--label (record)
  "Return the symbol for RECORD, or nil.
The kind of a blocked RECORD in `ghostel-terminal-list-blocked-symbols'
wins over its state in `ghostel-terminal-list-symbols'."
  (let ((state (plist-get record :state)))
    (or (and (eq state 'blocked)
             (alist-get (plist-get record :kind) ghostel-terminal-list-blocked-symbols))
        (alist-get state ghostel-terminal-list-symbols))))

(defvar ghostel-terminal-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ghostel-terminal-list-visit)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    (define-key map (kbd "o") #'ghostel-terminal-list-visit-other-window)
    (define-key map (kbd "n") #'ghostel-terminal-list-next-terminal)
    (define-key map (kbd "p") #'ghostel-terminal-list-previous-terminal)
    (define-key map (kbd "f") #'ghostel-terminal-list-next-item)
    (define-key map (kbd "b") #'ghostel-terminal-list-previous-item)
    (define-key map (kbd "N") #'ghostel-status-next)
    (define-key map (kbd "t") #'ghostel-terminal-list-toggle-attention-only)
    (define-key map (kbd "P") #'ghostel-terminal-list-toggle-project)
    (define-key map (kbd "d") #'ghostel-terminal-list-toggle-full-directory)
    (define-key map (kbd "?") #'ghostel-terminal-list-toggle-keys)
    (dotimes (i 9)
      (define-key map (kbd (format "M-%d" (1+ i)))
                  #'ghostel-terminal-list-visit-nth-terminal))
    map)
  "Keymap for `ghostel-terminal-list-mode'.")

(defvar-local ghostel-terminal-list--attention-only nil
  "Non-nil while this terminal list shows only terminals that need you.")

(defvar-local ghostel-terminal-list--full-directory nil
  "Non-nil while this terminal list shows each whole directory on its own line.")

(defvar-local ghostel-terminal-list--project-only nil
  "Non-nil while this terminal list shows only the project's terminals.
The project is that of `ghostel-terminal-list--directory'.")

(defvar-local ghostel-terminal-list--directory nil
  "Directory of the buffer this terminal list was last opened from.")

(defvar-local ghostel-terminal-list--show-keys nil
  "Non-nil while this terminal list shows its keys above the terminals.")

(defvar-local ghostel-terminal-list--items nil
  "Start position and (BUFFER . ID) of every item, in buffer order.
ID is nil for a terminal heading.")

(defun ghostel-terminal-list--age (since)
  "Return SINCE as a short age like \"5m\", or nil when SINCE is nil."
  (when since
    (let ((secs (max 0 (floor (- (float-time) since)))))
      (cond ((< secs 60) (format "%ds" secs))
            ((< secs 3600) (format "%dm" (/ secs 60)))
            ((< secs 86400) (format "%dh" (/ secs 3600)))
            (t (format "%dd" (/ secs 86400)))))))

(defun ghostel-terminal-list--state-text (record)
  "Return the state text for RECORD, led by its symbol."
  (let ((state (plist-get record :state))
        (kind (plist-get record :kind))
        (progress (plist-get record :progress)))
    (propertize (concat (when-let* ((symbol (ghostel-terminal-list--label record)))
                          (concat symbol " "))
                        (symbol-name state)
                        (and kind (format ": %s" kind))
                        (and progress (format " %d%%" progress)))
                'face (ghostel-status--face state))))

(defun ghostel-terminal-list--parent (id ids)
  "Return the nearest ancestor of ID among IDS, or nil.
The root record \"\" is the ancestor of every other id."
  (unless (string-empty-p id)
    (let ((parent id)
          found)
      (while (and (not found) (string-search "/" parent))
        (setq parent (substring parent 0 (string-match "/[^/]*\\'" parent)))
        (when (member parent ids)
          (setq found parent)))
      (or found (and (member "" ids) "")))))

(defun ghostel-terminal-list--insert-item (key text)
  "Insert TEXT as the heading line of the item KEY."
  (push (cons (point) key) ghostel-terminal-list--items)
  (insert text "\n"))

(defun ghostel-terminal-list--insert-record (entry parent level)
  "Insert the (BUFFER ID RECORD SINCE) ENTRY at nesting LEVEL.
PARENT is the id of the record it nests under, or nil.  The record's
app is left out when PARENT has the same one, its own or inherited."
  (pcase-let* ((`(,buf ,id ,record ,since) entry)
               (indent (make-string (* 2 level) ?\s))
               (label (cond ((string-empty-p id) nil)
                            ((member parent '(nil "")) id)
                            (t (substring id (1+ (length parent))))))
               (age (ghostel-terminal-list--age since))
               (app (let ((app (plist-get record :app)))
                      (unless (and parent
                                   (equal app (with-current-buffer buf
                                                (ghostel-status-app parent))))
                        app)))
               (msg (plist-get record :msg)))
    (ghostel-terminal-list--insert-item
     (cons buf id)
     (concat indent
             (string-join
              (delq nil (list (ghostel-terminal-list--state-text record)
                              (and label (propertize label 'face 'bold))
                              (plist-get record :title)
                              (and app (propertize app 'face 'shadow))
                              (and age (propertize age 'face 'shadow))))
              "  ")))
    (when msg
      (let ((prefix (concat indent "    ")))
        (dolist (line (split-string msg "\n"))
          (insert (propertize (concat prefix line "\n")
                              'wrap-prefix prefix)))))))

(defun ghostel-terminal-list--short-host (host)
  "Return HOST up to its first dot, or all of it when it is an IP address."
  (if (or (string-search ":" host)                               ; IPv6
          (string-match-p (rx bos (+ (any digit ".")) eos) host)) ; IPv4
      host
    (car (split-string host "\\."))))

(define-button-type 'ghostel-terminal-list-terminal
  'action #'ghostel-terminal-list--visit-button
  'follow-link t
  'face 'bold)

(define-button-type 'ghostel-terminal-list-directory
  'action #'ghostel-terminal-list--dired-button
  'follow-link t
  'face 'ghostel-terminal-list-directory)

(defun ghostel-terminal-list--directory (buf &optional full)
  "Return the last component of BUF's directory, like \"src\".
A remote directory leads with its host, shortened by
`ghostel-terminal-list--short-host', like \"box:src\".  With FULL,
return the whole directory instead, abbreviated when local.  It is a
button that opens the directory in Dired, with the face
`ghostel-terminal-list-directory' and the whole directory as its help
echo."
  (let* ((dir (buffer-local-value 'default-directory buf))
         ;; Parsing the name never connects; abbreviating a remote one may
         ;; query TRAMP, so only local names are abbreviated.
         (host (file-remote-p dir 'host))
         (path (if host (file-local-name dir) (abbreviate-file-name dir)))
         (name (file-name-nondirectory (directory-file-name path)))
         (whole (if host dir path)))
    (make-text-button (if full
                          whole
                        (concat (when (and host (not (string-empty-p host)))
                                  (concat (ghostel-terminal-list--short-host host) ":"))
                                (if (string-empty-p name) path name)))
                      nil
                      'type 'ghostel-terminal-list-directory
                      'help-echo whole)))

(defun ghostel-terminal-list--insert-terminal (buf entries number)
  "Insert the heading of terminal BUF and its ENTRIES as a tree.
The heading shows NUMBER, like \"1\", in the `shadow' face, BUF's
name, padded to `ghostel-terminal-list-name-width' columns, its title and
the last component of its directory; while
`ghostel-terminal-list--full-directory', the whole directory instead
shows on a line below the records.  The name and the directory are buttons that
visit BUF and open the directory.  ENTRIES are BUF's (BUFFER ID RECORD
SINCE), most urgent first; each record nests under its nearest existing
ancestor."
  (let* ((name (buffer-name buf))
         (rest (concat
                (when-let* ((title (buffer-local-value 'ghostel-title buf)))
                  (concat "  " (propertize title 'face 'shadow)))
                (unless ghostel-terminal-list--full-directory
                  (concat "  " (ghostel-terminal-list--directory buf))))))
    (ghostel-terminal-list--insert-item
     (cons buf nil)
     (concat (propertize number 'face 'shadow) " "
             (make-text-button name nil 'type 'ghostel-terminal-list-terminal)
             (unless (string-empty-p rest)
               (make-string (max 0 (- ghostel-terminal-list-name-width
                                      (string-width name)))
                            ?\s))
             rest)))
  (unless entries
    (insert (propertize "  no status\n" 'face 'shadow)))
  (let ((ids (mapcar #'cadr entries)))
    (cl-labels ((insert-children (parent level)
                  (dolist (entry entries)
                    (when (equal (ghostel-terminal-list--parent (nth 1 entry) ids)
                                 parent)
                      (ghostel-terminal-list--insert-record entry parent level)
                      (insert-children (nth 1 entry) (1+ level))))))
      (insert-children nil 1)))
  (when ghostel-terminal-list--full-directory
    (insert (propertize (concat "  " (ghostel-terminal-list--directory buf t) "\n")
                        'wrap-prefix "  ")))
  (insert "\n"))

(defun ghostel-terminal-list--keys-text ()
  "Return the terse list of keys shown above the terminals.
The keys are in the face `help-key-binding', the rest in `shadow'."
  (let ((text (substitute-command-keys
               (format "\\<ghostel-terminal-list-mode-map>\
\\[ghostel-terminal-list-visit] visit  \
\\[ghostel-terminal-list-visit-other-window] other window  \
%s visit Nth  \
\\[ghostel-status-next] next needing you
\\[ghostel-terminal-list-next-terminal]/\
\\[ghostel-terminal-list-previous-terminal] terminal  \
\\[ghostel-terminal-list-next-item]/\
\\[ghostel-terminal-list-previous-item] record  \
\\[forward-button]/%s button  \
\\[revert-buffer] refresh
\\[ghostel-terminal-list-toggle-attention-only] needs you only  \
\\[ghostel-terminal-list-toggle-project] project  \
\\[ghostel-terminal-list-toggle-full-directory] full dirs  \
\\[describe-mode] all keys  \
\\[ghostel-terminal-list-toggle-keys] hide  \
\\[quit-window] quit"
                       (propertize "M-1…M-9" 'face 'help-key-binding)
                       (propertize "S-TAB" 'face 'help-key-binding)))))
    (add-face-text-property 0 (length text) 'shadow t text)
    text))

(defun ghostel-terminal-list--key-at (pos)
  "Return the key of the item whose block holds POS, or nil."
  (cdr (cl-find-if (lambda (item) (<= (car item) pos))
                   (reverse ghostel-terminal-list--items))))

(defun ghostel-terminal-list--project-root ()
  "Return the root of the project the list was opened from, or nil."
  (when-let* ((dir ghostel-terminal-list--directory)
              (project (let ((default-directory dir))
                         (project-current))))
    (project-root project)))

(defun ghostel-terminal-list--project-name (root)
  "Return the name the terminal list shows for the project at ROOT."
  (file-name-nondirectory (directory-file-name root)))

(defun ghostel-terminal-list--in-project (bufs root)
  "Return those of BUFS in the project at ROOT, in order.
Membership is that of `ghostel-project-buffer-list', so
`ghostel-project-buffer-scope' applies."
  (let ((members (let ((default-directory root))
                   (ghostel-project-buffer-list))))
    (cl-remove-if-not (lambda (buf) (memq buf members)) bufs)))

(defun ghostel-terminal-list--render (&rest _)
  "Redraw the terminal list, keeping point and each window on its item.
Terminals come most urgent first, by their most urgent record, then
those without records by name; only those that need you while
`ghostel-terminal-list--attention-only', and only those in the project
the list was opened from while `ghostel-terminal-list--project-only'.
They are numbered from 1, the numbers M-1 to M-9 visit, right-aligned
so the names line up.  While `ghostel-terminal-list--show-keys', the
keys come first."
  (let* ((windows (mapcar (lambda (win)
                            (cons win (ghostel-terminal-list--key-at (window-point win))))
                          (get-buffer-window-list nil nil t)))
         (key (ghostel-terminal-list--key-at (point)))
         (entries (ghostel-terminal-list--entries))
         (with-records (delete-dups (mapcar #'car entries)))
         (bufs (if ghostel-terminal-list--attention-only
                   (delete-dups
                    (mapcar #'car (cl-remove-if-not
                                   (lambda (entry)
                                     (memq (plist-get (nth 2 entry) :state)
                                           ghostel-status-notify-states))
                                   entries)))
                 (append with-records
                         (cl-remove-if (lambda (buf) (memq buf with-records))
                                       (ghostel-buffer-list)))))
         (root (and ghostel-terminal-list--project-only
                    (ghostel-terminal-list--project-root)))
         (bufs (if root (ghostel-terminal-list--in-project bufs root) bufs))
         (name (and root (ghostel-terminal-list--project-name root)))
         (inhibit-read-only t)
         (find (lambda (key)
                 (or (car (rassoc key ghostel-terminal-list--items))
                     (car (rassoc (cons (car key) nil) ghostel-terminal-list--items))
                     (caar ghostel-terminal-list--items)
                     (point-min)))))
    (erase-buffer)
    (setq ghostel-terminal-list--items nil)
    (setq mode-line-process
          (and (or ghostel-terminal-list--attention-only name)
               (concat (and ghostel-terminal-list--attention-only " needs you")
                       (and name (format " in %s" name)))))
    (when ghostel-terminal-list--show-keys
      (insert (ghostel-terminal-list--keys-text) "\n\n"))
    (if (null bufs)
        (insert (propertize (format (if ghostel-terminal-list--attention-only
                                        "No terminal%s needs you.\n"
                                      "No ghostel terminals%s.\n")
                                    (if name (format " in %s" name) ""))
                            'face 'shadow))
      (cl-loop with width = (length (number-to-string (length bufs)))
               for buf in bufs
               for n from 1
               do (ghostel-terminal-list--insert-terminal
                   buf (cl-remove-if-not (lambda (entry) (eq (car entry) buf)) entries)
                   (string-pad (number-to-string n) width nil t))))
    (setq ghostel-terminal-list--items (nreverse ghostel-terminal-list--items))
    (goto-char (funcall find key))
    (pcase-dolist (`(,win . ,key) windows)
      (set-window-point win (funcall find key)))))

(define-derived-mode ghostel-terminal-list-mode special-mode "Ghostel Terminals"
  "Major mode showing the program status of every ghostel terminal.
Each terminal lists its records, children indented under their
parent, each with its message below.
\\<ghostel-terminal-list-mode-map>\
\\[ghostel-terminal-list-next-terminal] and \
\\[ghostel-terminal-list-previous-terminal] move between terminals, \
\\[ghostel-terminal-list-next-item] and \
\\[ghostel-terminal-list-previous-item] also stop at each record.
\\[ghostel-terminal-list-visit] visits the terminal, M-1 to M-9 the terminals \
numbered 1 to 9.
\\[forward-button] and \\[backward-button] move between the terminal names
and directories, buttons that visit the terminal and open the directory
in Dired.
\\[ghostel-terminal-list-toggle-attention-only] switches between all terminals \
and only
those that need you, \\[ghostel-terminal-list-toggle-project] between all \
terminals and only those in the
project of the buffer the list was opened from.
\\[ghostel-terminal-list-toggle-full-directory] switches between the last \
component of each terminal's
directory and the whole directory below its records.
\\[ghostel-terminal-list-toggle-keys] shows or hides a terse list of these keys \
above the terminals.  The
buffer updates as records change.

\\{ghostel-terminal-list-mode-map}"
  (setq-local revert-buffer-function #'ghostel-terminal-list--render)
  (setq ghostel-terminal-list--attention-only ghostel-terminal-list-attention-only)
  (setq ghostel-terminal-list--full-directory ghostel-terminal-list-full-directory)
  (visual-line-mode 1))

(defun ghostel-terminal-list-toggle-attention-only ()
  "Switch between showing every terminal and only those that need you."
  (interactive)
  (setq ghostel-terminal-list--attention-only (not ghostel-terminal-list--attention-only))
  (ghostel-terminal-list--render)
  (message (if ghostel-terminal-list--attention-only
               "Showing terminals that need you"
             "Showing all terminals")))

(defun ghostel-terminal-list-toggle-project ()
  "Switch between showing every terminal and only the project's.
The project is that of the buffer the list was opened from, and
`ghostel-project-buffer-scope' decides which terminals belong to it."
  (interactive)
  (let ((root (or (ghostel-terminal-list--project-root)
                  (user-error "No current project"))))
    (setq ghostel-terminal-list--project-only (not ghostel-terminal-list--project-only))
    (ghostel-terminal-list--render)
    (if ghostel-terminal-list--project-only
        (message "Showing terminals in %s"
                 (ghostel-terminal-list--project-name root))
      (message "Showing all terminals"))))

(defun ghostel-terminal-list-toggle-full-directory ()
  "Switch between the last component of each directory and the whole one.
The whole directory shows on its own line below the terminal's records."
  (interactive)
  (setq ghostel-terminal-list--full-directory (not ghostel-terminal-list--full-directory))
  (ghostel-terminal-list--render)
  (message (if ghostel-terminal-list--full-directory
               "Showing whole directories"
             "Showing directory names")))

(defun ghostel-terminal-list-toggle-keys ()
  "Show or hide a terse list of the keys above the terminals."
  (interactive)
  (setq ghostel-terminal-list--show-keys (not ghostel-terminal-list--show-keys))
  (ghostel-terminal-list--render))

(defun ghostel-terminal-list--move (n items)
  "Move forward N of ITEMS, back when N is negative.
ITEMS are entries of `ghostel-terminal-list--items', in buffer order;
movement stops at the last one in that direction."
  (let ((items (if (< n 0) (reverse items) items))
        (past (if (< n 0) #'< #'>)))
    (dotimes (_ (abs n))
      (when-let* ((item (cl-find-if (lambda (item)
                                      (funcall past (car item) (point)))
                                    items)))
        (goto-char (car item))))))

(defun ghostel-terminal-list--terminal-items ()
  "Return the terminal headings of `ghostel-terminal-list--items'.
A heading's key has a nil id, a record's a string."
  (cl-remove-if #'cddr ghostel-terminal-list--items))

(defun ghostel-terminal-list-next-terminal (&optional n)
  "Move forward N terminal headings."
  (interactive "p")
  (ghostel-terminal-list--move (or n 1) (ghostel-terminal-list--terminal-items)))

(defun ghostel-terminal-list-previous-terminal (&optional n)
  "Move back N terminal headings."
  (interactive "p")
  (ghostel-terminal-list--move (- (or n 1)) (ghostel-terminal-list--terminal-items)))

(defun ghostel-terminal-list-next-item (&optional n)
  "Move forward N terminal or record headings."
  (interactive "p")
  (ghostel-terminal-list--move (or n 1) ghostel-terminal-list--items))

(defun ghostel-terminal-list-previous-item (&optional n)
  "Move back N terminal or record headings."
  (interactive "p")
  (ghostel-terminal-list--move (- (or n 1)) ghostel-terminal-list--items))

(defun ghostel-terminal-list--terminal-at-point ()
  "Return the live terminal buffer of the item at point."
  (let ((buf (car (ghostel-terminal-list--key-at (point)))))
    (unless (buffer-live-p buf)
      (user-error "No terminal at point"))
    buf))

(defun ghostel-terminal-list-visit ()
  "Switch to the terminal of the item at point.
Quit the list's window first, so a window popped up for the list closes
and the terminal shows in the window the list was opened from."
  (interactive)
  (let ((buf (ghostel-terminal-list--terminal-at-point)))
    (quit-window)
    (pop-to-buffer buf (append display-buffer--same-window-action
                               '((category . comint))))))

(defun ghostel-terminal-list--visit-button (button)
  "Switch to the terminal of BUTTON's heading."
  (goto-char (button-start button))
  (ghostel-terminal-list-visit))

(defun ghostel-terminal-list--dired-button (button)
  "Open the directory of the terminal of BUTTON's heading in Dired.
Quit the list's window first, like `ghostel-terminal-list-visit'."
  (goto-char (button-start button))
  (let ((dir (buffer-local-value 'default-directory
                                 (ghostel-terminal-list--terminal-at-point))))
    (quit-window)
    (dired dir)))

(defun ghostel-terminal-list-visit-other-window ()
  "Show the terminal of the item at point in another window."
  (interactive)
  (switch-to-buffer-other-window (ghostel-terminal-list--terminal-at-point)))

(defun ghostel-terminal-list-visit-nth-terminal (n)
  "Switch to the Nth terminal of the terminal list, counting from 1.
Interactively, N is the digit of the key that ran the command, so M-1
visits the first terminal and M-9 the ninth; from other keys it is the
prefix argument."
  (interactive
   (let ((key (event-basic-type last-command-event)))
     (list (if (and (characterp key) (<= ?1 key ?9))
               (- key ?0)
             (prefix-numeric-value current-prefix-arg)))))
  (let ((item (and (> n 0) (nth (1- n) (ghostel-terminal-list--terminal-items)))))
    (unless item
      (user-error "No terminal %d" n))
    (goto-char (car item))
    (ghostel-terminal-list-visit)))

;;;###autoload
(defun ghostel-terminal-list ()
  "Show the program status of every ghostel terminal, most urgent first.
Point goes to the most urgent terminal, at the top.  Status is tracked
from the first call on, so ages show for changes after that.  In the
list, \\<ghostel-terminal-list-mode-map>\\[ghostel-terminal-list-toggle-project] \
shows only the terminals of the current buffer's project."
  (interactive)
  (ghostel-terminal-list--start)
  (let ((dir (and (not (derived-mode-p 'ghostel-terminal-list-mode))
                  default-directory))
        (buf (get-buffer-create ghostel-terminal-list-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'ghostel-terminal-list-mode)
        (ghostel-terminal-list-mode))
      ;; Opened from the list itself, it keeps its project.
      (when dir
        (setq ghostel-terminal-list--directory dir))
      (ghostel-terminal-list--render))
    (pop-to-buffer buf)
    (goto-char (or (caar ghostel-terminal-list--items) (point-min)))))

(provide 'ghostel-terminal-list)
;;; ghostel-terminal-list.el ends here
