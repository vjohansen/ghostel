;;; ghostel-dabbrev-completion-test.el --- Tests for ghostel: insert a word from the scrollback  -*- lexical-binding: t; -*-

;;; Commentary:

;; `ghostel-dabbrev-completion--words', which collects the words in a
;; buffer, `ghostel-dabbrev-completion--prefix', which finds the word
;; begun at the cursor, and `ghostel-dabbrev-completion', which types
;; the one picked into the terminal.
;; All tests are pure elisp — no native module required.

;;; Code:

(require 'ghostel-test-helpers)
(require 'ghostel-dabbrev-completion)

(ert-deftest ghostel-dabbrev-completion-test-collects-words ()
  "Words count, the last one first.
Boundary characters split words, so paths and hostnames stay whole."
  (with-temp-buffer
    (insert "ls ~/src/foo/bar.txt short words\n"
            "something-longer: api.example.com\n")
    (should (equal '("api.example.com" "something-longer" "words" "short"
                     "~/src/foo/bar.txt")
                   (ghostel-dabbrev-completion--words
                    (point-min) (point-max))))))

(ert-deftest ghostel-dabbrev-completion-test-cleans-words ()
  "Duplicates go, trailing periods are dropped, rules without letters skip."
  (with-temp-buffer
    (insert "Compiling.... ghostel-module done\n"
            "──────────────\n"
            "ghostel-module.\n")
    (should (equal '("ghostel-module" "done" "Compiling")
                   (ghostel-dabbrev-completion--words
                    (point-min) (point-max))))))

(ert-deftest ghostel-dabbrev-completion-test-trims-sentence-marks ()
  "Trailing `!' and `?' go like periods."
  (with-temp-buffer
    (insert "Built ghostel-module! Is it-installed?\n")
    (should (equal '("it-installed" "ghostel-module" "Built")
                   (ghostel-dabbrev-completion--words
                    (point-min) (point-max))))))

(ert-deftest ghostel-dabbrev-completion-test-min-length ()
  "Words of any kind need `ghostel-dabbrev-completion-min-length' characters."
  (with-temp-buffer
    (insert "12345 123 e61dbf7 ab a.c MINGW64\n")
    (should (equal '("MINGW64" "a.c" "e61dbf7" "123" "12345")
                   (ghostel-dabbrev-completion--words
                    (point-min) (point-max))))
    (let ((ghostel-dabbrev-completion-min-length 5))
      (should (equal '("MINGW64" "e61dbf7" "12345")
                     (ghostel-dabbrev-completion--words
                      (point-min) (point-max)))))))

(ert-deftest ghostel-dabbrev-completion-test-keeps-urls-whole ()
  "URLs keep their scheme, port and balanced parentheses.
Quotes or brackets around one stay out, and a word running into one
keeps only its part before it."
  (with-temp-buffer
    (insert "see https://example.com/issues/1?x=1&y=2 for details.\n"
            "'https://example.com/quoted' (https://w.org/Rust_(language))\n"
            "--location=https://example.com/x ssh://git@host/repo.git\n")
    (should (equal '("ssh://git@host/repo.git" "https://example.com/x"
                     "--location=" "https://w.org/Rust_(language)"
                     "https://example.com/quoted" "details" "for"
                     "https://example.com/issues/1?x=1&y=2" "see")
                   (ghostel-dabbrev-completion--words
                    (point-min) (point-max))))))

(ert-deftest ghostel-dabbrev-completion-test-colon-before-slash ()
  "A `:' before a slash joins a word, but a `:LINE' tail does not.
So drive paths, URLs of any scheme and `host:/path' stay whole."
  (with-temp-buffer
    (insert "error in C:\\src\\main.c:12:3\n"
            "--prefix=C:/tools/emacs file:///D:/a.txt user@host:/tmp/x\n")
    (should (equal '("user@host:/tmp/x" "file:///D:/a.txt"
                     "--prefix=C:/tools/emacs" "C:\\src\\main.c" "error")
                   (ghostel-dabbrev-completion--words
                    (point-min) (point-max))))))

(ert-deftest ghostel-dabbrev-completion-test-keeps-variables-whole ()
  "A `$' reference stays on its name, also PowerShell's `$env:NAME'.
A `$' before anything else splits a word."
  (with-temp-buffer
    (insert "ls $HOME/bin $env:USERPROFILE\\src ~/src$ $(pwd)\n")
    (should (equal '("pwd" "~/src" "$env:USERPROFILE\\src" "$HOME/bin")
                   (ghostel-dabbrev-completion--words
                    (point-min) (point-max))))))

(ert-deftest ghostel-dabbrev-completion-test-skips-osc133-prompts ()
  "Prompt text marked by OSC 133 counts nowhere, also after a soft wrap."
  (with-temp-buffer
    (insert "output /wrapped/long" (propertize "\n" 'ghostel-wrap t) "/path.txt\n"
            (propertize "user@host ~/src/project\n$ " 'ghostel-prompt t)
            "cd ~/src/elsewhere\n"
            (propertize "user@host ~/src/elsewhere\n$ " 'ghostel-prompt t))
    (should (equal '("~/src/elsewhere" "/wrapped/long/path.txt" "output")
                   (ghostel-dabbrev-completion--words
                    (point-min) (point-max))))))

(ert-deftest ghostel-dabbrev-completion-test-joins-soft-wraps ()
  "A word split by a wrap newline counts whole, also across the range start."
  (with-temp-buffer
    (insert "ls /very/long" (propertize "\n" 'ghostel-wrap t) "/path.txt\n")
    (should (equal '("/very/long/path.txt")
                   (ghostel-dabbrev-completion--words
                    (point-min) (point-max))))
    (goto-char (point-min))
    (forward-line 1)
    (should (equal '("/very/long/path.txt")
                   (ghostel-dabbrev-completion--words (point) (point-max))))))

(ert-deftest ghostel-dabbrev-completion-test-end-at-line-start ()
  "An end at the start of a line leaves that line out."
  (with-temp-buffer
    (insert "first-line-word\nsecond-line-word\n")
    (goto-char (point-min))
    (should (equal '("first-line-word")
                   (ghostel-dabbrev-completion--words
                    (point-min) (line-beginning-position 2))))))

(ert-deftest ghostel-dabbrev-completion-test-prefix ()
  "The word typed before a position is found, split like the others.
Text after the position, a space before it, and prompts do not count;
a word after `=' starts there."
  (with-temp-buffer
    (insert "$ cd mo")
    (should (equal "mo" (ghostel-dabbrev-completion--prefix (point-max))))
    (insert "nkey-suggested")
    (should (equal "mo" (ghostel-dabbrev-completion--prefix
                         (- (point-max) 14))))
    (insert " ")
    (should-not (ghostel-dabbrev-completion--prefix (point-max)))
    (dolist (typed '("C:\\Us" "$env:US" "https://exa" "~/src/fo"))
      (erase-buffer)
      (insert "ls '" typed)
      (should (equal typed (ghostel-dabbrev-completion--prefix (point-max)))))
    (erase-buffer)
    (insert "make --file=/tmp/mo")
    (should (equal "/tmp/mo" (ghostel-dabbrev-completion--prefix (point-max))))
    (insert " --flag=")
    (should-not (ghostel-dabbrev-completion--prefix (point-max)))
    (erase-buffer)
    (insert (propertize "~/src$" 'ghostel-prompt t) "mo")
    (should (equal "mo" (ghostel-dabbrev-completion--prefix (point-max))))
    (should (equal "o" (ghostel-dabbrev-completion--prefix
                        (point-max) (1- (point-max)))))))

(ert-deftest ghostel-dabbrev-completion-test-prefix-across-wrap ()
  "A word typed across a soft wrap counts whole."
  (with-temp-buffer
    (insert "$ cd /very/lo" (propertize "\n" 'ghostel-wrap t) "ng/pa")
    (should (equal "/very/long/pa"
                   (ghostel-dabbrev-completion--prefix (point-max))))))

(defmacro ghostel-dabbrev-completion-test--with-terminal (mode text &rest body)
  "Run BODY in a ghostel buffer in input MODE showing TEXT.
The terminal cursor is at the end of TEXT, and `ghostel-paste-string'
and `ghostel-readonly-exit' only record their calls in `pasted' and
`exited'."
  (declare (indent 2))
  `(with-temp-buffer
     (setq major-mode 'ghostel-mode)
     (setq-local ghostel--input-mode ,mode)
     (insert ,text)
     (setq-local ghostel--cursor-char-pos (point-max))
     (let (pasted exited)
       (cl-letf (((symbol-function 'ghostel-paste-string)
                  (lambda (s) (push s pasted)))
                 ((symbol-function 'ghostel-readonly-exit)
                  (lambda () (setq exited t))))
         ,@body))))

(ert-deftest ghostel-dabbrev-completion-test-pastes-choice ()
  "The choice is pasted; candidates keep their order, lowest first."
  (ghostel-dabbrev-completion-test--with-terminal 'semi-char
      "cd ~/src/project\nmake something-long\n$ "
    (let (candidates sort)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (setq candidates (all-completions "" table)
                         sort (completion-metadata-get
                               (completion-metadata "" table nil)
                               'display-sort-function))
                   "~/src/project")))
        (ghostel-dabbrev-completion))
      (should (equal '("something-long" "make" "~/src/project") candidates))
      (should (eq #'identity sort))
      (should (equal '("~/src/project") pasted))
      (should-not exited))))

(ert-deftest ghostel-dabbrev-completion-test-leaves-copy-mode ()
  "In copy mode the word is pasted after leaving it."
  (ghostel-dabbrev-completion-test--with-terminal 'copy "cd ~/src/project\n"
    (ghostel-dabbrev-completion)
    (should exited)
    (should (equal '("~/src/project") pasted))))

(ert-deftest ghostel-dabbrev-completion-test-line-mode-inserts ()
  "In line mode the word goes into the input, at its end when point is outside."
  (ghostel-dabbrev-completion-test--with-terminal 'line
      "cd ~/src/project\n$ ls "
    (setq-local ghostel--line-input-start (copy-marker (- (point-max) 3)))
    (setq-local ghostel--line-input-end (copy-marker (point-max) t))
    (goto-char (point-min))
    (ghostel-dabbrev-completion)
    (should (equal "cd ~/src/project\n$ ls ~/src/project"
                   (buffer-string)))
    (should-not pasted)))

(ert-deftest ghostel-dabbrev-completion-test-ignores-window ()
  "Wherever the window is, all words count, the lowest first."
  (ghostel-dabbrev-completion-test--with-terminal 'semi-char
      "cd ~/src/scrolled-back\nmake something-long\n$ "
    (let (candidates)
      ;; The window is scrolled back to show only the first line.
      (cl-letf (((symbol-function 'window-start)
                 (lambda (&rest _) (point-min)))
                ((symbol-function 'window-end)
                 (lambda (&rest _)
                   (save-excursion
                     (goto-char (point-min))
                     (line-beginning-position 2))))
                ((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (setq candidates (all-completions "" table))
                   (car candidates))))
        (ghostel-dabbrev-completion))
      (should (equal '("something-long" "make" "~/src/scrolled-back")
                     candidates)))))

(ert-deftest ghostel-dabbrev-completion-test-completes-typed-word ()
  "A word begun at the cursor is the initial input and is not typed again.
Only words that start with it are offered."
  (ghostel-dabbrev-completion-test--with-terminal 'semi-char
      "echo monkey money demo-mode\n$ ls mo"
    (let (candidates initial)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table _pred _require input &rest _)
                   (setq candidates (all-completions "" table)
                         initial input)
                   "money")))
        (ghostel-dabbrev-completion))
      (should (equal "mo" initial))
      (should (equal '("money" "monkey") candidates))
      (should (equal '("ney") pasted)))))

(ert-deftest ghostel-dabbrev-completion-test-skips-typed-word ()
  "The word being typed is not offered, though it shows."
  (ghostel-dabbrev-completion-test--with-terminal 'semi-char
      "ls ~/src/monkey ~/src/money\n$ ls ~/src/mo"
    (let (candidates)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (setq candidates (all-completions "" table))
                   (car candidates))))
        (ghostel-dabbrev-completion))
      (should (equal '("~/src/money" "~/src/monkey") candidates))
      (should (equal '("ney") pasted)))))

(ert-deftest ghostel-dabbrev-completion-test-types-sole-word ()
  "When only one word fits, it is typed without asking."
  (ghostel-dabbrev-completion-test--with-terminal 'semi-char
      "echo monkey money\n$ ls monk"
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (error "Asked"))))
      (ghostel-dabbrev-completion))
    (should (equal '("ey") pasted))))

(ert-deftest ghostel-dabbrev-completion-test-no-word-starts-with-typed ()
  "Without a word that starts with what is typed there is nothing to pick.
Words with its letters elsewhere, as `flex' matches them, do not count."
  (ghostel-dabbrev-completion-test--with-terminal 'semi-char
      "echo monkey\n$ mky"
    (should-error (ghostel-dabbrev-completion) :type 'user-error)
    (should-not pasted)))

(ert-deftest ghostel-dabbrev-completion-test-flex-fallback ()
  "Minibuffer input matches words by prefix, or failing that with `flex'."
  (ghostel-dabbrev-completion-test--with-terminal 'semi-char
      "echo monkey money\n$ "
    (let (matches)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (let ((completion-styles '(basic)))
                     (dolist (input '("e" "mky"))
                       (let ((all (completion-all-completions
                                   input table nil (length input))))
                         (when all (setcdr (last all) nil))
                         (push (sort all #'string<) matches))))
                   "monkey")))
        (ghostel-dabbrev-completion))
      ;; `e' does not reach `money' and `monkey', as `basic' found `echo'.
      (should (equal '(("monkey") ("echo")) matches)))))

(ert-deftest ghostel-dabbrev-completion-test-line-mode-completes-typed-word ()
  "In line mode the rest of the word picked is inserted at point."
  (ghostel-dabbrev-completion-test--with-terminal 'line
      "echo monkey money\n$ ls mo x"
    (setq-local ghostel--line-input-start (copy-marker (- (point-max) 7)))
    (setq-local ghostel--line-input-end (copy-marker (point-max) t))
    (goto-char (- (point-max) 2))
    (let (initial)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _table _pred _require input &rest _)
                   (setq initial input)
                   "monkey")))
        (ghostel-dabbrev-completion))
      (should (equal "mo" initial)))
    (should (equal "echo monkey money\n$ ls monkey x" (buffer-string)))))

(ert-deftest ghostel-dabbrev-completion-test-no-words ()
  "Without a long enough word the command signals a `user-error'."
  (ghostel-dabbrev-completion-test--with-terminal 'semi-char "$ ls\n"
    (should-error (ghostel-dabbrev-completion) :type 'user-error)))

(ert-deftest ghostel-dabbrev-completion-test-key ()
  "\\`C-c TAB' runs the command, also from semi-char mode."
  (should (eq #'ghostel-dabbrev-completion
              (lookup-key ghostel-mode-map (kbd "C-c TAB"))))
  (should (eq #'ghostel-dabbrev-completion
              (lookup-key ghostel-semi-char-mode-map (kbd "C-c TAB")))))

(provide 'ghostel-dabbrev-completion-test)
;;; ghostel-dabbrev-completion-test.el ends here
