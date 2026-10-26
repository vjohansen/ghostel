;;; ghostel-terminal-list-test.el --- Tests for ghostel: terminal list -*- lexical-binding: t; -*-

;;; Commentary:

;; The cross-terminal view of program status records: ordering and the
;; terminal list buffer.
;; All tests are pure elisp — no native module required.

;;; Code:

(require 'ghostel-test-helpers)
(require 'ghostel-terminal-list)

(defmacro ghostel-terminal-list-test--with-bufs (names &rest body)
  "Run BODY with fake ghostel buffers bound to the symbols in NAMES.
Buffer A is in directory /a/, and so on.  Status changes reach only the
terminal list tracker; its state and the hooks it installs start empty and
every buffer is killed afterwards."
  (declare (indent 1))
  `(let ((ghostel-program-status-functions '(ghostel-terminal-list--track))
         (ghostel-mode-hook nil)
         (kill-buffer-hook nil)
         (ghostel-terminal-list--since (make-hash-table :test #'equal))
         (ghostel-terminal-list--timer nil)
         ,@(mapcar (lambda (name)
                     `(,name (let ((buf (generate-new-buffer
                                         ,(format "*ghostel-%s*" name))))
                               (with-current-buffer buf
                                 (setq major-mode 'ghostel-mode
                                       default-directory ,(format "/%s/" name)))
                               buf)))
                   names))
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (name) `(when (buffer-live-p ,name) (kill-buffer ,name)))
                 names)
       (when-let* ((list-buf (get-buffer ghostel-terminal-list-buffer-name)))
         (kill-buffer list-buf))
       (when (timerp ghostel-terminal-list--timer)
         (cancel-timer ghostel-terminal-list--timer)))))

(defun ghostel-terminal-list-test--report (buf state &optional id kind msg)
  "Apply a report with STATE, ID, KIND and MSG in BUF."
  (with-current-buffer buf
    (ghostel--osc-program-status state id kind nil nil nil msg)))

(defun ghostel-terminal-list-test--order (&optional states)
  "Return (BUFFER-NAME ID) of `ghostel-terminal-list--entries' for STATES."
  (mapcar (lambda (e) (list (buffer-name (car e)) (nth 1 e)))
          (ghostel-terminal-list--entries states)))

(ert-deftest ghostel-terminal-list-test-orders-by-state-then-age ()
  "Records sort by urgency, then the longest waiting first."
  (ghostel-terminal-list-test--with-bufs (a b)
    (let ((now 1000.0))
      (cl-letf (((symbol-function 'float-time) (lambda (&rest _) now)))
        (ghostel-terminal-list-test--report a "done" "x")
        (setq now 1001.0)
        (ghostel-terminal-list-test--report b "working")
        (ghostel-terminal-list-test--report b "blocked" "q" "question")
        (setq now 1002.0)
        (ghostel-terminal-list-test--report a "blocked" "p" "permission")
        (setq now 1003.0)
        (ghostel-terminal-list-test--report b "done" "y")))
    (should (equal '(("*ghostel-b*" "q") ("*ghostel-a*" "p")
                     ("*ghostel-a*" "x") ("*ghostel-b*" "y")
                     ("*ghostel-b*" ""))
                   (ghostel-terminal-list-test--order)))
    (should (equal '(("*ghostel-b*" "q") ("*ghostel-a*" "p"))
                   (ghostel-terminal-list-test--order '(blocked))))))

(ert-deftest ghostel-terminal-list-test-since-follows-state-changes ()
  "The time is kept while the state stays, renewed on change, gone on removal."
  (ghostel-terminal-list-test--with-bufs (a)
    (let ((now 10.0))
      (cl-letf (((symbol-function 'float-time) (lambda (&rest _) now)))
        (ghostel-terminal-list-test--report a "working" "x")
        (setq now 20.0)
        (ghostel-terminal-list-test--report a "working" "x" nil "still")
        (should (= 10.0 (gethash (cons a "x") ghostel-terminal-list--since)))
        (ghostel-terminal-list-test--report a "blocked" "x")
        (should (= 20.0 (gethash (cons a "x") ghostel-terminal-list--since)))
        (ghostel-terminal-list-test--report a "clear" "x")
        (should-not (gethash (cons a "x") ghostel-terminal-list--since))))))

(ert-deftest ghostel-terminal-list-test-dead-buffers-pruned ()
  "A killed terminal leaves neither entries nor tracked times."
  (ghostel-terminal-list-test--with-bufs (a b)
    (ghostel-terminal-list-test--report a "done")
    (ghostel-terminal-list-test--report b "done")
    (kill-buffer a)
    (should (equal '(("*ghostel-b*" "")) (ghostel-terminal-list-test--order)))
    (should (= 1 (hash-table-count ghostel-terminal-list--since)))))

(defmacro ghostel-terminal-list-test--in-list (&rest body)
  "Open the terminal list at a fixed time and run BODY in it."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'float-time) (lambda (&rest _) 100.0)))
     (save-window-excursion
       (ghostel-terminal-list)
       (with-current-buffer ghostel-terminal-list-buffer-name
         ,@body))))

(defun ghostel-terminal-list-test--text ()
  "Return the terminal list buffer text without properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(ert-deftest ghostel-terminal-list-test-buffer-shows-records ()
  "Each record shows with its message below; item movement stops on each."
  (ghostel-terminal-list-test--with-bufs (a)
    (cl-letf (((symbol-function 'float-time) (lambda (&rest _) 100.0)))
      (ghostel-terminal-list-test--report a "blocked" nil "permission" "Run rm?")
      (ghostel-terminal-list-test--report a "done" "build"))
    (ghostel-terminal-list-test--in-list
      (should (equal (concat "1 *ghostel-a*       a\n"
                             "  🔒 blocked: permission  0s\n"
                             "      Run rm?\n"
                             "    ✅ done  build  0s\n"
                             "\n")
                     (ghostel-terminal-list-test--text)))
      (goto-char (point-min))
      (ghostel-terminal-list-next-item)
      (should (equal (cons a "") (ghostel-terminal-list--key-at (point))))
      (ghostel-terminal-list-next-item)
      (should (equal (cons a "build") (ghostel-terminal-list--key-at (point))))
      (ghostel-terminal-list-previous-item 2)
      (should (equal (cons a nil) (ghostel-terminal-list--key-at (point)))))))

(ert-deftest ghostel-terminal-list-test-terminal-movement-skips-records ()
  "`n' and `p' move between terminal headings, past records and children."
  (ghostel-terminal-list-test--with-bufs (a b)
    (ghostel-terminal-list-test--report a "blocked" nil "permission")
    (ghostel-terminal-list-test--report a "done" "build")
    (ghostel-terminal-list-test--report b "done")
    (ghostel-terminal-list-test--in-list
      (goto-char (point-min))
      (should (equal (cons a nil) (ghostel-terminal-list--key-at (point))))
      (ghostel-terminal-list-next-terminal)
      (should (equal (cons b nil) (ghostel-terminal-list--key-at (point))))
      (ghostel-terminal-list-next-terminal)
      (should (equal (cons b nil) (ghostel-terminal-list--key-at (point))))
      (ghostel-terminal-list-next-item)
      (should (equal (cons b "") (ghostel-terminal-list--key-at (point))))
      (ghostel-terminal-list-previous-terminal)
      (should (equal (cons b nil) (ghostel-terminal-list--key-at (point))))
      (ghostel-terminal-list-next-terminal -1)
      (should (equal (cons a nil) (ghostel-terminal-list--key-at (point))))
      (ghostel-terminal-list-previous-terminal -1)
      (should (equal (cons b nil) (ghostel-terminal-list--key-at (point)))))))

(ert-deftest ghostel-terminal-list-test-meta-digit-visits-nth-terminal ()
  "M-1 to M-9 switch to the terminal of that number in the list."
  (ghostel-terminal-list-test--with-bufs (a b)
    (ghostel-terminal-list-test--report a "done")
    (ghostel-terminal-list-test--report b "blocked" "q" "question")
    (should (eq #'ghostel-terminal-list-visit-nth-terminal
                (lookup-key ghostel-terminal-list-mode-map (kbd "M-9"))))
    (ghostel-terminal-list-test--in-list
      (let ((last-command-event ?\M-2))
        (call-interactively #'ghostel-terminal-list-visit-nth-terminal))
      (should (eq a (window-buffer))))
    (ghostel-terminal-list-test--in-list
      (let ((last-command-event ?\M-1))
        (call-interactively #'ghostel-terminal-list-visit-nth-terminal))
      (should (eq b (window-buffer))))
    (ghostel-terminal-list-test--in-list
      (should-error (ghostel-terminal-list-visit-nth-terminal 3) :type 'user-error))))

(ert-deftest ghostel-terminal-list-test-visit-closes-popped-up-window ()
  "RET in a list that split the frame closes its window again.
The terminal shows in the window the list was opened from."
  (ghostel-terminal-list-test--with-bufs (a)
    (ghostel-terminal-list-test--report a "done")
    (save-window-excursion
      (delete-other-windows)
      (let ((origin (selected-window)))
        (let ((display-buffer-overriding-action '(display-buffer-pop-up-window)))
          (ghostel-terminal-list))
        (should-not (one-window-p))
        (ghostel-terminal-list-visit)
        (should (one-window-p))
        (should (eq origin (selected-window)))
        (should (eq a (window-buffer)))))))

(ert-deftest ghostel-terminal-list-test-buffer-nests-children ()
  "Records nest under their nearest existing ancestor, labelled relative to it.
A message keeps its lines."
  (ghostel-terminal-list-test--with-bufs (a)
    (with-current-buffer a
      (ghostel--osc-program-status "working" "deploy" nil nil nil "Rollout" "Deploying")
      (ghostel--osc-program-status "blocked" "deploy/eu" "permission" nil nil nil
                                   "Approve\nsecond line")
      (ghostel--osc-program-status "error" "deploy/eu/db" nil nil "psql" nil nil)
      (ghostel--osc-program-status "done" "x/y/z" nil nil nil nil nil))
    (ghostel-terminal-list-test--in-list
      (should (equal (concat "1 *ghostel-a*       a\n"
                             "  ✅ done  x/y/z  0s\n"
                             "  ⏳ working  deploy  Rollout  0s\n"
                             "      Deploying\n"
                             "    🔒 blocked: permission  eu  0s\n"
                             "        Approve\n"
                             "        second line\n"
                             "      ❌ error  db  psql  0s\n"
                             "\n")
                     (ghostel-terminal-list-test--text))))))

(ert-deftest ghostel-terminal-list-test-child-hides-parent-app ()
  "A child shows its app only when it differs from its parent's.
The parent's app may be inherited from further up."
  (ghostel-terminal-list-test--with-bufs (a)
    (with-current-buffer a
      (ghostel--osc-program-status "working" nil nil nil "claude-code" nil nil)
      (ghostel--osc-program-status "working" "agent" nil nil "claude-code" nil nil)
      (ghostel--osc-program-status "working" "agent/sub" nil nil nil nil nil)
      (ghostel--osc-program-status "working" "agent/sub/tool" nil nil "claude-code"
                                   nil nil)
      (ghostel--osc-program-status "working" "other" nil nil "codex" nil nil))
    (ghostel-terminal-list-test--in-list
      (should (equal (concat "1 *ghostel-a*       a\n"
                             "  ⏳ working  claude-code  0s\n"
                             "    ⏳ working  agent  0s\n"
                             "      ⏳ working  sub  0s\n"
                             "        ⏳ working  tool  0s\n"
                             "    ⏳ working  other  codex  0s\n"
                             "\n")
                     (ghostel-terminal-list-test--text))))))

(ert-deftest ghostel-terminal-list-test-heading-shows-title-and-directory ()
  "A heading has the number, the padded name, the title, then the directory.
The number is in the `shadow' face.  The name pads to
`ghostel-terminal-list-name-width' columns.  The directory shows its last
component, with its own face and the whole directory as help echo; a
root directory shows whole."
  (ghostel-terminal-list-test--with-bufs (a)
    (with-current-buffer a
      (setq ghostel-title "Claude"
            default-directory "/src/shop/"))
    (ghostel-terminal-list-test--in-list
      (should (string-prefix-p "1 *ghostel-a*       Claude  shop\n"
                               (ghostel-terminal-list-test--text)))
      (should (eq 'shadow (get-text-property (point-min) 'face)))
      (goto-char (point-min))
      (search-forward "shop")
      (should (equal "/src/shop/" (get-text-property (1- (point)) 'help-echo)))
      (should (eq 'ghostel-terminal-list-directory
                  (get-text-property (1- (point)) 'face)))
      (let ((ghostel-terminal-list-name-width 13))
        (ghostel-terminal-list--render)
        (should (string-prefix-p "1 *ghostel-a*    Claude  shop\n"
                                 (ghostel-terminal-list-test--text))))
      (with-current-buffer a
        (setq default-directory "/")
        (rename-buffer "*ghostel-longer-name*"))
      (ghostel-terminal-list--render)
      (should (string-prefix-p "1 *ghostel-longer-name*  Claude  /\n"
                               (ghostel-terminal-list-test--text))))))

(ert-deftest ghostel-terminal-list-test-tab-moves-between-name-and-directory ()
  "TAB and S-TAB move between the terminal names and directories.
The buttons keep the heading's faces and highlight under the mouse."
  (ghostel-terminal-list-test--with-bufs (a b)
    (ghostel-terminal-list-test--report a "done")
    (ghostel-terminal-list-test--in-list
      (let ((tab (lookup-key ghostel-terminal-list-mode-map (kbd "TAB")))
            (backtab (lookup-key ghostel-terminal-list-mode-map (kbd "<backtab>")))
            (stops nil))
        (goto-char (point-min))
        (dotimes (_ 4)
          (funcall tab 1)
          (push (buffer-substring-no-properties (point) (button-end (button-at (point))))
                stops))
        (should (equal '("*ghostel-a*" "a" "*ghostel-b*" "b") (nreverse stops)))
        (funcall backtab 1)
        (should (equal "*ghostel-b*" (button-label (button-at (point)))))
        (should (eq 'bold (get-text-property (point) 'face)))
        (should (eq 'highlight (get-text-property (point) 'mouse-face)))
        (funcall tab 1)
        (should (eq 'ghostel-terminal-list-directory (get-text-property (point) 'face)))
        (should (eq 'highlight (get-text-property (point) 'mouse-face)))))))

(ert-deftest ghostel-terminal-list-test-buttons-visit-and-open-dired ()
  "The name button visits its terminal; the directory button opens Dired there."
  (ghostel-terminal-list-test--with-bufs (a b)
    (ghostel-terminal-list-test--report a "done")
    (ghostel-terminal-list-test--in-list
      (goto-char (point-min))
      (search-forward "*ghostel-b*")
      (push-button (1- (point)))
      (should (eq b (window-buffer))))
    (ghostel-terminal-list-test--in-list
      (let (opened)
        (cl-letf (((symbol-function 'dired) (lambda (dir) (setq opened dir))))
          (goto-char (point-min))
          (search-forward "*ghostel-b*")
          (push-button (1- (line-end-position))))
        (should (equal "/b/" opened))))))

(ert-deftest ghostel-terminal-list-test-heading-numbers-align ()
  "From ten terminals on, numbers right-align so the names line up."
  (ghostel-terminal-list-test--with-bufs (a b c d e f g h i j)
    (ghostel-terminal-list-test--in-list
      (let ((text (ghostel-terminal-list-test--text)))
        (should (string-prefix-p " 1 *ghostel-a*" text))
        (should (string-search "\n 9 *ghostel-i*" text))
        (should (string-search "\n10 *ghostel-j*" text))))))

(ert-deftest ghostel-terminal-list-test-heading-shows-remote-host ()
  "A remote directory leads with its host's first label, or a whole IP address.
Its help echo is the whole remote directory."
  (ghostel-terminal-list-test--with-bufs (a)
    (ghostel-terminal-list-test--in-list
      (pcase-dolist (`(,dir ,shown) '(("/ssh:box:/srv/shop/" "box:shop")
                                      ("/ssh:box:/" "box:/")
                                      ("/ssh:u@box.example.com:/home/u/" "box:u")
                                      ("/ssh:10.0.0.5:/srv/" "10.0.0.5:srv")))
        (with-current-buffer a
          (setq default-directory dir))
        (ghostel-terminal-list--render)
        (should (string-prefix-p (concat "1 *ghostel-a*       " shown "\n")
                                 (ghostel-terminal-list-test--text)))
        (goto-char (point-min))
        (should (equal dir (get-text-property (1- (line-end-position))
                                              'help-echo)))))))

(ert-deftest ghostel-terminal-list-test-toggle-full-directory ()
  "The toggle moves the directory below the records and shows it whole.
The whole directory is a button that opens Dired, and a remote one keeps
its method and host."
  (ghostel-terminal-list-test--with-bufs (a)
    (with-current-buffer a
      (setq default-directory "/src/shop/"))
    (ghostel-terminal-list-test--report a "done")
    (should (eq #'ghostel-terminal-list-toggle-full-directory
                (lookup-key ghostel-terminal-list-mode-map (kbd "d"))))
    (ghostel-terminal-list-test--in-list
      (ghostel-terminal-list-toggle-full-directory)
      (should (equal (concat "1 *ghostel-a*\n"
                             "  ✅ done  0s\n"
                             "  /src/shop/\n"
                             "\n")
                     (ghostel-terminal-list-test--text)))
      (let (opened)
        (cl-letf (((symbol-function 'dired) (lambda (dir) (setq opened dir))))
          (goto-char (point-min))
          (search-forward "/src/shop/")
          (push-button (1- (point))))
        (should (equal "/src/shop/" opened))))
    (ghostel-terminal-list-test--in-list
      (should (string-search "\n  /src/shop/\n" (ghostel-terminal-list-test--text)))
      (with-current-buffer a
        (setq default-directory "/ssh:u@box.example.com:/home/u/"))
      (ghostel-terminal-list--render)
      (should (string-search "\n  /ssh:u@box.example.com:/home/u/\n"
                             (ghostel-terminal-list-test--text)))
      (ghostel-terminal-list-toggle-full-directory)
      (should (string-prefix-p "1 *ghostel-a*       box:u\n  ✅"
                               (ghostel-terminal-list-test--text))))
    ;; A new terminal list starts from the option.
    (kill-buffer ghostel-terminal-list-buffer-name)
    (let ((ghostel-terminal-list-full-directory t))
      (ghostel-terminal-list-test--in-list
        (should (string-prefix-p "1 *ghostel-a*\n  ✅ done  0s\n  /ssh:"
                                 (ghostel-terminal-list-test--text)))))))

(ert-deftest ghostel-terminal-list-test-toggle-keys ()
  "`?' shows the keys above the terminals and hides them again.
Point stays on its terminal; reopening the list puts it on the first one."
  (ghostel-terminal-list-test--with-bufs (a b)
    (should (eq #'ghostel-terminal-list-toggle-keys
                (lookup-key ghostel-terminal-list-mode-map (kbd "?"))))
    (ghostel-terminal-list-test--in-list
      (ghostel-terminal-list-next-terminal)
      (ghostel-terminal-list-toggle-keys)
      (should (string-prefix-p (concat "RET visit  o other window  M-1…M-9 visit Nth"
                                       "  N next needing you\n"
                                       "n/p terminal  f/b record  TAB/S-TAB button"
                                       "  g refresh\n"
                                       "t needs you only  d full dirs  h all keys  ? hide"
                                       "  q quit\n"
                                       "\n"
                                       "1 *ghostel-a*")
                               (ghostel-terminal-list-test--text)))
      (should (eq 'help-key-binding (car (get-text-property (point-min) 'face))))
      (should (equal (cons b nil) (ghostel-terminal-list--key-at (point))))
      (ghostel-terminal-list)
      (should (equal (cons a nil) (ghostel-terminal-list--key-at (point))))
      (ghostel-terminal-list-toggle-keys)
      (should (string-prefix-p "1 *ghostel-a*" (ghostel-terminal-list-test--text))))))

(ert-deftest ghostel-terminal-list-test-opens-at-top ()
  "Showing the list puts point on the most urgent terminal, at the top."
  (ghostel-terminal-list-test--with-bufs (a b)
    (ghostel-terminal-list-test--report a "done")
    (ghostel-terminal-list-test--report b "blocked")
    (ghostel-terminal-list-test--in-list
      (should (equal (cons b nil) (ghostel-terminal-list--key-at (point))))
      (ghostel-terminal-list-next-item 2)
      (should (equal (cons a nil) (ghostel-terminal-list--key-at (point))))
      (ghostel-terminal-list)
      (should (equal (cons b nil) (ghostel-terminal-list--key-at (point)))))))

(ert-deftest ghostel-terminal-list-test-buffer-orders-terminals-and-refreshes ()
  "Terminals come most urgent first; a status change redraws via the timer."
  (ghostel-terminal-list-test--with-bufs (a b)
    (ghostel-terminal-list-test--report a "done")
    (ghostel-terminal-list-test--report b "blocked")
    (ghostel-terminal-list-test--in-list
      (should (string-match-p "\\`1 \\*ghostel-b\\*       b\n.*\n\n2 \\*ghostel-a\\*"
                              (ghostel-terminal-list-test--text)))
      (ghostel-terminal-list-test--report b "clear")
      (should (timerp ghostel-terminal-list--timer))
      (ghostel-terminal-list--refresh)
      (should-not ghostel-terminal-list--timer)
      ;; A terminal without records follows, by name.
      (should (equal (concat "1 *ghostel-a*       a\n"
                             "  ✅ done  0s\n"
                             "\n"
                             "2 *ghostel-b*       b\n"
                             "  no status\n"
                             "\n")
                     (ghostel-terminal-list-test--text))))))

(ert-deftest ghostel-terminal-list-test-buffer-follows-terminals-opening-and-closing ()
  "Terminals without records appear when opened and go when killed."
  (ghostel-terminal-list-test--with-bufs (a b)
    (ghostel-terminal-list-test--in-list
      (should (string-search "*ghostel-b*" (ghostel-terminal-list-test--text)))
      (kill-buffer b)
      (should (timerp ghostel-terminal-list--timer))
      (ghostel-terminal-list--refresh)
      (should-not (string-search "*ghostel-b*" (ghostel-terminal-list-test--text)))
      (let ((c (generate-new-buffer "*ghostel-c*")))
        (unwind-protect
            (progn
              (with-current-buffer c
                (setq major-mode 'ghostel-mode)
                (run-hooks 'ghostel-mode-hook))
              (should (timerp ghostel-terminal-list--timer))
              (ghostel-terminal-list--refresh)
              (should (string-search "*ghostel-c*" (ghostel-terminal-list-test--text))))
          (kill-buffer c))))))

(ert-deftest ghostel-terminal-list-test-toggle-attention-only ()
  "The toggle shows only terminals that need you, with all their records."
  (ghostel-terminal-list-test--with-bufs (a b c)
    (ghostel-terminal-list-test--report a "working" "build")
    (ghostel-terminal-list-test--report a "blocked" "build/test" "question" "Rerun?")
    (ghostel-terminal-list-test--report b "working")
    (ghostel-terminal-list-test--in-list
      (should (string-search "*ghostel-b*" (ghostel-terminal-list-test--text)))
      (should (string-search "*ghostel-c*" (ghostel-terminal-list-test--text)))
      (ghostel-terminal-list-toggle-attention-only)
      (should (equal " needs you" mode-line-process))
      (should (equal (concat "1 *ghostel-a*       a\n"
                             "  ⏳ working  build  0s\n"
                             "    💬 blocked: question  test  0s\n"
                             "        Rerun?\n"
                             "\n")
                     (ghostel-terminal-list-test--text)))
      (ghostel-terminal-list-test--report a "clear")
      (ghostel-terminal-list--refresh)
      (should (equal "No terminal needs you.\n" (ghostel-terminal-list-test--text)))
      (ghostel-terminal-list-toggle-attention-only)
      (should-not mode-line-process)
      (should (string-search "*ghostel-c*" (ghostel-terminal-list-test--text))))
    ;; A new terminal list starts from the option; an open one keeps its toggle.
    (kill-buffer ghostel-terminal-list-buffer-name)
    (let ((ghostel-terminal-list-attention-only t))
      (ghostel-terminal-list-test--in-list
        (should (equal "No terminal needs you.\n" (ghostel-terminal-list-test--text)))))))

(provide 'ghostel-terminal-list-test)
;;; ghostel-terminal-list-test.el ends here
