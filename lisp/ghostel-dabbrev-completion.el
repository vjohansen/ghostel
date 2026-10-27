;;; ghostel-dabbrev-completion.el --- Type a word shown in a ghostel terminal -*- lexical-binding: t; -*-

;; Author: Vagn Johansen <gonz808@hotmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `ghostel-dabbrev-completion' picks a word the terminal shows, such
;; as a path, URL, branch or hash it just printed, and types it at the
;; terminal cursor, so it need not be selected and copied first.  The
;; words come from the whole scrollback, the lowest first.  A word
;; begun at the cursor is completed: what is typed of it is the
;; initial input, and is not typed again.  When only one word fits,
;; it is typed without asking.

;;; Code:

(require 'subr-x)
(require 'ghostel)


;;; Customization

(defcustom ghostel-dabbrev-completion-min-length 3
  "Fewest characters of a word that `ghostel-dabbrev-completion' offers."
  :type 'natural
  :group 'ghostel)

;; Match minibuffer input by prefix, and with `flex' when nothing
;; matches.  Change it for category `ghostel-word' in
;; `completion-category-overrides'.
(add-to-list 'completion-category-defaults '(ghostel-word (styles basic flex)))


;;; Collecting words

(defvar ghostel-dabbrev-completion--history nil
  "Minibuffer history for `ghostel-dabbrev-completion'.")

(defun ghostel-dabbrev-completion--spans (text)
  "Return the words in TEXT as (BEG . END) offsets, ordered by BEG.
A word is a run of characters that are neither whitespace nor in
`ghostel-word-boundary-string', as for double-click, except that a
`:' before a slash or backslash, as in `C:\\Users', `file:///x' and
`host:/tmp', and a `$' starting a variable, as in `$HOME' and
`$env:PATH', are part of it.  An http(s) URL, as `ghostel--url-regex'
matches it, is one word whatever it holds, so a port or a balanced
`(...)' in it does not split it."
  (let* ((char `(not (any blank ?\n
                          ,@(string-to-list ghostel-word-boundary-string))))
         (word-regexp
          (rx-to-string `(+ (or ,char
                                (seq ":" (any "/\\"))
                                (seq "$" (? (+ alpha) ":") (any alpha "_"))))
                        t))
         (pos 0)
         urls
         words)
    (while (string-match ghostel--url-regex text pos)
      (let* ((match-beg (match-beginning 0))
             (match-end (match-end 0))
             (before (and (> match-beg 0) (aref text (1- match-beg)))))
        ;; A URL in quotes or brackets, as in 'https://x', can end with
        ;; the closing one.
        (when (and before
                   (eq (aref text (1- match-end))
                       (alist-get before '((?' . ?') (?` . ?`)
                                           (?\[ . ?\]) (?{ . ?})))))
          (setq match-end (1- match-end)))
        (push (cons match-beg match-end) urls)
        (setq pos match-end)))
    (setq pos 0)
    ;; Split the text between URLs; a word running into one keeps only
    ;; its part before it, such as `--url=' in `--url=https://x'.
    (dolist (url (nreverse (cons (cons (length text) nil) urls)))
      (let ((gap-end (car url)))
        (while (and (< pos gap-end)
                    (string-match word-regexp text pos)
                    (< (match-beginning 0) gap-end))
          (push (cons (match-beginning 0) (min (match-end 0) gap-end)) words)
          (setq pos (match-end 0)))
        (when (cdr url)
          (push url words)
          (setq pos (max pos (cdr url))))))
    (nreverse words)))

(defun ghostel-dabbrev-completion--words (beg end)
  "Return the distinct words between BEG and END, the last one first.
The words are those of `ghostel-dabbrev-completion--spans' that do not
start in a prompt, text OSC 133 marked as one.  Trailing periods, `!'
and `?' are dropped, and only words with a letter or digit and at
least `ghostel-dabbrev-completion-min-length' characters count, so
rules like \"────────\" do not.  A word that soft-wraps across rows
counts whole, also where it crosses BEG or END.

`ghostel-prompt-regexp' is not used: it matches output such as
`src/a.c:1: x > y' as a prompt, which would lose `src/a.c'."
  (let* ((limit ghostel--soft-wrap-row-limit)
         (start (ghostel--soft-wrap-line-beginning beg limit))
         ;; END is just past the last character in range, often the
         ;; start of the next line.
         (stop (ghostel--soft-wrap-line-end (max beg (1- end)) limit))
         (joined (ghostel--wrap-joined-region start stop limit))
         (text (car joined))
         result)
    (dolist (word (ghostel-dabbrev-completion--spans text))
      (let ((string (string-trim-right
                     (substring text (car word) (cdr word)) "[.!?]+")))
        ;; The prompt test is the costly one, so it goes last.
        (when (and (>= (length string) ghostel-dabbrev-completion-min-length)
                   (string-match-p "[[:alnum:]]" string)
                   (not (get-text-property
                         (ghostel--wrap-offset-to-pos (car word) (cdr joined))
                         'ghostel-prompt)))
          (push string result))))
    (delete-dups result)))

(defun ghostel-dabbrev-completion--prefix (pos &optional floor)
  "Return what is typed of a word before POS, or nil.
That is the last word of `ghostel-dabbrev-completion--spans' in POS's
line before POS, when it ends at POS, from after its last `=' as in
shell completion of `--file=/tmp/x'.  It does not reach back past
FLOOR or into a prompt, text OSC 133 marked as one.  Text after POS,
such as a suggestion the shell shows, does not count."
  (let* ((limit ghostel--soft-wrap-row-limit)
         (bound (max (ghostel--soft-wrap-line-beginning pos limit)
                     (or floor (point-min))))
         ;; Back to BOUND, or to where a prompt before POS ends.
         (start (if (get-text-property (max (1- pos) (point-min))
                                       'ghostel-prompt)
                    pos
                  (previous-single-property-change
                   pos 'ghostel-prompt nil bound))))
    (when (< start pos)
      (let ((text (car (ghostel--wrap-joined-region start pos limit))))
        (when-let* ((span (car (last (ghostel-dabbrev-completion--spans text))))
                    ((= (cdr span) (length text)))
                    (word (replace-regexp-in-string
                           (rx bos (* nonl) "=") ""
                           (substring text (car span))))
                    ((not (string-empty-p word))))
          word)))))


;;; Command

(defun ghostel-dabbrev-completion--target ()
  "Return where `ghostel-dabbrev-completion' types, as (POS . FLOOR), or nil.
In line mode POS is point, or the end of the input when point is
outside it, like `ghostel-line-mode-self-insert', and FLOOR the start
of the input.  Otherwise POS is the terminal cursor and FLOOR nil."
  (if (eq ghostel--input-mode 'line)
      (let ((start (and (markerp ghostel--line-input-start)
                        (marker-position ghostel--line-input-start)))
            (end (and (markerp ghostel--line-input-end)
                      (marker-position ghostel--line-input-end))))
        (cons (if (and start end (or (< (point) start) (> (point) end)))
                  end
                (point))
              start))
    (and ghostel--cursor-char-pos
         (<= ghostel--cursor-char-pos (point-max))
         (list ghostel--cursor-char-pos))))

;;;###autoload
(defun ghostel-dabbrev-completion ()
  "Pick a word from the scrollback and type it into the terminal.
Offers the words of `ghostel-dabbrev-completion--words' in the whole
buffer, the lowest first, wherever the window is.  The word is pasted
at the terminal cursor, after leaving copy or Emacs mode; in line mode
it is inserted at point in the input instead.

A word begun there, as `ghostel-dabbrev-completion--prefix' finds it,
is the initial input, only longer words that start with it are
offered, and only the rest of the word picked is typed.  When only
one word fits, it is typed without asking.  Input in the minibuffer
matches by the completion category `ghostel-word', which tries the
styles `basic' and then `flex'."
  (interactive)
  (ghostel--ensure-ghostel-buffer)
  (let* ((target (ghostel-dabbrev-completion--target))
         (pos (car target))
         (prefix (or (and pos (ghostel-dabbrev-completion--prefix
                               pos (cdr target)))
                     ""))
         (words (seq-filter
                 (lambda (word)
                   (and (string-prefix-p prefix word)
                        (> (length word) (length prefix))))
                 (ghostel-dabbrev-completion--words (point-min) (point-max)))))
    (unless words
      (user-error "No word%s in the scrollback"
                  (if (string-empty-p prefix)
                      ""
                    (format " starting with %S" prefix))))
    (let ((word (if (cdr words)
                    (completing-read
                     "Insert word: "
                     (lambda (string pred action)
                       (if (eq action 'metadata)
                           '(metadata (category . ghostel-word)
                                      (display-sort-function . identity)
                                      (cycle-sort-function . identity))
                         (complete-with-action action words string pred)))
                     nil t (and (not (string-empty-p prefix)) prefix)
                     'ghostel-dabbrev-completion--history)
                  (car words))))
      (cond
       ((string-empty-p word))
       ((eq ghostel--input-mode 'line)
        (unless (= (point) pos)
          (deactivate-mark)
          (goto-char pos))
        (insert (substring word (length prefix))))
       (t
        (when (memq ghostel--input-mode '(copy emacs))
          (ghostel-readonly-exit))
        (ghostel-paste-string (substring word (length prefix))))))))

(provide 'ghostel-dabbrev-completion)
;;; ghostel-dabbrev-completion.el ends here
