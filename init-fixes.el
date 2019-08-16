;;; Aligning Variables
;;

(defun align-variables (start end)
  "Attempts to align all variables in an assignment list or keys
    in a hash table. For instance:

      (\"org-mode\"
       :base-extension \"org\"
       :recursive t
       :headline-levels 4  ; Just the default for this project.
       :auto-sitemap t     ; Generate sitemap.org automagically
      )

    Turns into the following if the region begins on the first line
    with the colon:

      (\"org-mode\"
        :base-extension  \"org\"
        :recursive       t
        :headline-levels 4  ; Just the default for this project.
        :auto-sitemap    t     ; Generate sitemap.org automagically
      )

    Note: This currently does not align the comments.

    All lines in region will be indented to the position of the first
    line. For most languages/modes, this should be sufficient, but if
    it doesn't work, start the region as the column they should all
    be indented. For instance:

       var x = 10,
           start = beginningOfFile,
           end = File.end();

    Start the region at the x, to achieve:

       var x     = 10,
           start = beginningOfFile,
           end   = File.end();"
  (interactive "r")
  (save-excursion
    (goto-char start)
    (let* ((times (count-lines start end))
           (real-start (if (looking-at-p "[ \\t(]")
                           (1- (search-forward-regexp "[^ \\t(]" end t))
                         start))
           (real-end nil)  ;; Will be set later
           (dest-column (progn
                          (goto-char real-start)
                          (current-column))))

      ;; Step 1. Align all lines to the column of the text in the first line
      (dotimes (line times)
        (forward-line)
        (indent-line-to dest-column))
      (setq real-end (point))

      ;; Step 2. Align all the values in a second column
      (align-regexp real-start real-end "\\(\\s-*\\)\\(\\S-*\\)\\(\\s-*\\)" 3 1 nil))))


;;; Kill Entire Lines
;;

;; This creates a macro that moves to the beginning of the line and
;;   then calls a function given to it. Quite an interesting approach:
(defmacro bol-with-prefix (function)
  "Define a new function which calls FUNCTION.
    Except it moves to beginning of line before calling FUNCTION when
    called with a prefix argument. The FUNCTION still receives the
    prefix argument."
  (let ((name (intern (format "ha/%s-BOL" function))))
    `(progn
       (defun ,name (p)
         ,(format
           "Call `%s', but move to the beginning of the line when called with a prefix argument."
           function)
         (interactive "P")
         (when p
           (forward-line 0))
         (call-interactively ',function))
       ',name)))

;;And we re-bind them to functions that use them.
(global-set-key [remap paredit-kill] (bol-with-prefix paredit-kill))
(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))
(global-set-key [remap kill-line] (bol-with-prefix kill-line))

(global-set-key (kbd "C-k") (bol-with-prefix kill-line))


;;; Selecting Words
;;

;; While you can type: =M-4 M-@= to select the next four words, that
;;   is too much finger stretching for something I want to use often.
;;   (Yes, hitting =M-@= four times is actually easier, but...)

;;   The =M-w= does nothing different with a prefix, so this seems quite
;;   reasonable to re-purpose such a direct command:

(defun ha/select-words-or-copy (num)
  "If region is active, copy to kill ring as normal, but given a
    prefix, selects that number of words."
  (interactive "p")
  (cond
   ((use-region-p) (kill-ring-save (region-beginning) (region-end)))
   ((> num 0) (progn
                (beginning-of-thing 'word)
                (push-mark (point) nil t)
                (forward-word num)))
   ((< num 0) (progn
                (end-of-thing 'word)
                (push-mark (point) nil t)
                (forward-word num)))))

(global-set-key (kbd "M-w") 'ha/select-words-or-copy)

;;; Better Newline
;;

;; Since =paredit= and other modes automatically insert final
;;    characters like semi-colons and parenthesis, what I really want is
;;    to hit return from the /end of the line/. Pretty simple function.
(defun newline-for-code ()
  "Inserts a newline character, but from the end of the current line."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

;;And we can bind that to the free, /Meta-Return/:
;; NOTE: Remember, this works everywhere /except/ for org-mode.
(global-set-key (kbd "M-RET") 'newline-for-code)

;;; Join Lines
;;

;; I like how =M-SPC= removes all but one space, and =M-\= removes all
;;    spaces. Would be nice to remove all /newlines/ in the same way.

;;    Sure, =C-x C-o= removes all following newlines, so if at the end of
;;    the first line that should be /joined/, then this acts somewhat
;;    like =M-SPC=.

;; I would like to have =M-RET= remove the lines similar to the way
;;    =M-SPC= works, but that is already bound in =org-mode= to making a
;;    special header, so I'll just bind it to Control.

(defun join-lines ()
  "If at the end of the line, will join the following line to the
    end of this one...unless it is blank, in which case, it will
    keep joining lines until the next line with text is
    connected."
  (interactive)

  ;; Move to the the beginning of the white space before attempting
  ;; this process. This allows us to join lines even if we are in the
  ;; middle of some empty lines.
  (re-search-backward "[^[:space:]\\r\\n]")
  (forward-char)

  ;; Just in case we have some trailing whitespace we can't see, let's
  ;; just get rid of it. Won't do anything if in the middle of a line,
  ;; or if there is not trailing whitespace.
  (delete-trailing-whitespace (point) (point-at-eol))

  ;; While we are at the end of the line, join a line, remove the
  ;; whitespace, and keep on going until we're through...
  (while (eq (point-at-eol) (point))
    (delete-char 1)
    (delete-trailing-whitespace (point) (point-at-eol))))

(global-set-key (kbd "C-RET") 'join-lines)

;;; Better Movement
;;

;; The [[https://github.com/doitian/iy-go-to-char][iy-go-to-char]] project allows a quick search for a particular
;;    character. In [[http://www.youtube.com/watch?v%3DNXTf8_Arl1w][Episode 6]] of [[http://www.emacsrocks.com][EmacsRocks]], [[http://twitter.com/emacsrocks][Magnar Sveen]] pulls it all
;;    together and makes a compelling case for micro-optimizations.
;;    I find it better than =avy= when in a macro.

;; To use, type =C-`= and then a character, number or other symbol to
;;    jump to. Typing most things will bugger out of its "state" and
;;    start editing, however, typing:

;;    - =;= will jump to the next occurrence of that letter
;;    - =,= jumps backwards
;;    - =C-w= cuts from where the cursor started and where it ended.
;;    - =M-w= copies that region

(use-package iy-go-to-char
  :ensure t
  :bind
  ("C-`" . iy-go-to-char)
  ("<f13>" . iy-go-to-char)
  ("C-~" . iy-go-to-char-backward))


;;; Better Beginning of Line
;;

;; This [[http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/][Emacs Redux article]] has a great suggestion for having =C-a= go
;;    to the beginning of the line's content instead of the actual
;;    beginning of the line. Hit =C-a= a second to get to the actual
;;    beginning.
(defun smarter-move-beginning-of-line (arg)
    "Move point back to indentation of beginning of line.

  Move point to the first non-whitespace character on this line.
  If point is already there, move to the beginning of the line.
  Effectively toggle between the first non-whitespace character and
  the beginning of the line.

  If ARG is not nil or 1, move forward ARG - 1 lines first.  If
  point reaches the beginning or end of the buffer, stop there."
    (interactive "^p")
    (setq arg (or arg 1))

    ;; Move lines first
    (when (/= arg 1)
      (let ((line-move-visual nil))
        (forward-line (1- arg))))

    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (move-beginning-of-line 1))))
;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)
(global-set-key [remap org-beginning-of-line]  'smarter-move-beginning-of-line)


;;; Next and Previous File
;;

;; Sometimes it is obvious what is the /next file/ based on the one
;;    I'm currently reading. For instance, in my journal entries, the
;;    filename is a number that can be incremented. Same with
;;    presentation files...

(defun split-string-with-number (string)
  "Returns a list of three components of the string, the first is
  the text prior to any numbers, the second is the embedded number,
  and the third is the rest of the text in the string."
  (let* ((start (string-match "[0-9]+" string))
         (end (string-match "[^0-9]+" string start)))
    (if start
        (list (substring string 0 start)
              (substring string start end)
              (if end  (substring string end)  "")))))

;; Which means that the following defines this function:

(split-string-with-number "abc42xyz")  ;; ("abc" "42" "xyz")
(split-string-with-number "42xyz")     ;; ("" "42" "xyz")
(split-string-with-number "abc42")     ;; ("abc" "42" "")
(split-string-with-number "20140424")  ;; ("" "20140424" "")
(split-string-with-number "abcxyz")    ;; nil

;; Given this splitter function, we create a function that takes some
;;    sort of operator and return a new filename based on the conversion
;;    that happens:
(defun find-file-number-change (f)
  (let* ((filename (buffer-file-name))
         (parts    (split-string-with-number
                    (file-name-base filename)))
         (new-name (number-to-string
                    (funcall f (string-to-number (nth 1 parts))))))
    (concat (file-name-directory filename)
            (nth 0 parts)
            new-name
            (nth 2 parts))))

;; And this allows us to create two simple functions that can load the
;;    "next" and "previous" files:

(defun find-file-increment ()
  "Takes the current buffer, and loads the file that is 'one
  more' than the file contained in the current buffer. This
  requires that the current file contain a number that can be
  incremented."
  (interactive)
  (find-file (find-file-number-change '1+)))

(defun find-file-decrement ()
  "Takes the current buffer, and loads the file that is 'one
  less' than the file contained in the current buffer. This
  requires that the current file contain a number that can be
  decremented."
  (interactive)
  (find-file (find-file-number-change '1-)))

(global-set-key (kbd "C-c f +") 'find-file-increment)
(global-set-key (kbd "C-c f n") 'find-file-increment)
(global-set-key (kbd "C-c f -") 'find-file-decrement)
(global-set-key (kbd "C-c f p") 'find-file-decrement)

(provide 'init-fixes)
