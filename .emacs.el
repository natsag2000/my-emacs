;;; Nagi's Emacs configuration file
;;; -------------------------------
;;
;; shameless stolen from Howard Abrams
;;
;; TODO: one day do it with literal programming

;;; -------------------------------------------------------------------------
;;; START
;;; -------------------------------------------------------------------------

;;; GENERAL SETTINGS
;;; ----------------

;;; Directory location
;;
(defconst na/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))
(defun na/emacs-subdirectory (d) (expand-file-name d na/emacs-directory))

;;; Directory structure
;;
(let* ((subdirs '("elisp" "backups" "snippets" "ac-dict"))
       (fulldirs (mapcar (lambda (d) (na/emacs-subdirectory d)) subdirs)))
  (dolist (dir fulldirs)
    (when (not (file-exists-p dir))
      (message "Make directory: %s" dir)
      (make-directory dir))))

;;; Customization file
;;
;; settings in ui, menu will be saved here
;;
(setq custom-file (expand-file-name "custom.el" na/emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; Setting up the Load Path
;;
;; extra packages not available via the package manager go in my
;; personal stach at: $HOME/.emacs.d/elisp
;;
(add-to-list 'load-path (na/emacs-subdirectory "elisp"))
(add-to-list 'load-path "~/elisp")
(require 'cl)
(require 'init-support)


;;; PACKAGE INITIALIZATION
;;; -----------------------

;;; Package manager
;;
(require 'package)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages")))

(setq package-pinned-packages '((async . "melpa")
                                (magit . "melpa-stable")
                                (magit-popup . "melpa-stable")
                                (org . "org")))
(package-initialize)

;; solution until all converted to use-package
(defun packages-install (packages)
  "Given a list of packages, this will install them from the standard locations."
  (let ((to-install (inverse-filter 'package-installed-p packages)))
    (when to-install
      (package-refresh-contents)
      (dolist (it to-install)
        (package-install it)
        (delete-other-windows)))))

;;; Use-Package
;;
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)





;;; VARIABLES
;;; ---------
(setq user-full-name "Natsagdorj Shagdar (nagi)")
(if (equal "nagi-ESPRIMO-P520" system-name)
    (setq user-mail-address "n.shagdar@dvz-mv.de")
  (setq user-mail-address "natsag2000@gmail.com"))

;;; Tabs vs Spaces
;; Indent-only-with-spaces
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; make tab key to indent first then completion
(setq-default tab-always-indent 'complete)


;;; Encrypting Files
;;
;; on mac install gpg
;; brew install gpg

;; now, any file loaded with a gpg extension will prompt password
(setq epa-file-select-keys 2)
;; you can have emacs cache th password. not sure i do...
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;;; DISPLAY SETTINGS
;;; ----------------
;;
(setq initial-scratch-message "") ;; Uh, I know what Scratch is for
(setq visible-bell t)             ;; Get rid of the beeps
(when (window-system)
  (tool-bar-mode 0)               ;; Toolbars were only cool with XEmacs
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (scroll-bar-mode -1))            ;; Scrollbars are waste screen estate

;;; Mode Line
;;
(require 'init-mode-line)

;;; Whitespace Mode
;;
(use-package whitespace
  :bind ("C-c T w" . whitespace-mode)
  :init
  (setq whitespace-line-column nil
        whitespace-display-mappings '((space-mark 32 [183] [46])
                                      (newline-mark 10 [9166 10])
                                      (tab-mark 9 [9654 9] [92 9])))
  :config
  (set-face-attribute 'whitespace-space       nil :foreground "#666666" :background nil)
  (set-face-attribute 'whitespace-newline     nil :foreground "#666666" :background nil)
  (set-face-attribute 'whitespace-indentation nil :foreground "#666666" :background nil)
  :diminish whitespace-mode)


;;; Fill Mode
;;
(use-package fill
  :bind ("C-c T f" . auto-fill-mode)
  :init (add-hook 'org-mode-hook 'turn-on-auto-fill)
  :diminish auto-fill-mode)

;;; -------------------------------------------------------------------------
;;; KEY BINDINGS
;;; -------------------------------------------------------------------------

;;; Hydra Sequences
;;
(use-package hydra
  :ensure t
  :config
  (hydra-add-font-lock))

;; Easily manipulate the size of the windows using the arrow keys in a
;; particular buffer window.
;; Other Hydra sequences are associated with the package they control.
(require 'windmove)

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defhydra hydra-splitter (global-map "<f9>")
  "splitter"
  ("<left>" hydra-move-splitter-left)
  ("<down>" hydra-move-splitter-down)
  ("<up>" hydra-move-splitter-up)
  ("<right>" hydra-move-splitter-right))

;;; Displaying Command Sequences
;;
;; hydra makes good! but useful for built-in key sequences
;;
(use-package guide-key
  :ensure t
  :init    (setq guide-key/guide-key-sequence
                 '("C-x r"     ; rectanges and registers
                   "C-x 4"     ; window commands
                   "M-s h"     ; hi-lock highlighting
                   "C-x w"     ; alternative to M-s ...
                   "C-c @"     ; hs-hide-show mode
                   "C-c p"     ; projectile
                   "C-x j"     ; bookmark+
                   "C-x C-z"   ; Zoom window
                   "C-x"       ; general
                   "<f2>"
                   "<f9>"
                   (org-mode "C-c C-x")))
  :config  (guide-key-mode 1)
  :diminish guide-key-mode)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/highlight-command-regexp
      '("rectangle"
        ("register" . font-lock-type-face)
        ("bookmark" . "hot pink")))

;; Emacs has never seen a need for function keys, and I agree...for
;; the most part. For things really /away from the flow/, they don't
;; seem to bad. But what are those?

;; - *F1* - Help? Isn't Control-H good enough?
;; - *F2* - Special odd, little-used characters that I have to think
;;          about before remembering what its binding.
;; - *F3* - Define a keyboard macro
;; - *F4* - Replay a keyboard macro
;; - *F5* - Use org-mode’s Mark Ring feature globally
;; - *F6* - Open to temporary, changeable commands...
;; - *F7* - Switch to another window ... Control goes the other way.
;; - *F8* - Switch to buffer
;; - *F9* - My extension (replacement?) for =C-c= for changing colors
;;   and other odd bindings that I actually don't use that often.

(global-set-key (kbd "<f5>") 'org-mark-ring-push)
(global-set-key (kbd "C-<f5>") 'org-mark-ring-goto)
(global-set-key (kbd "<f7>") 'other-window)
(global-set-key (kbd "C-<f7>") (lambda () (interactive) (other-window -1)))

;;; F2 and F9 Helpers
;;
(define-prefix-command 'personal-global-map)
(global-set-key (kbd "<f9>") 'personal-global-map)

;;(require 'init-f2) ;; TODO

;;; Highlighting and Narrowing
;;
;; I like the ability to highlight random text.

;;    - =M-s h .= :: highlight-symbol-at-point
;;    - =M-s h l= :: highlight-lines-matching-regexp
;;    - =M-s h p= :: highlight-phrase
;;    - =M-s h r= :: highlight-regexp
;;    - =M-s h u= :: unhighlight-regexp
(defun ha/highlite-logs ()
  "Highlight certain lines in specific files. Currently, only log files are supported."
  (interactive)
  (when (equal "log" (file-name-extension (buffer-file-name)))
    (hi-lock-mode 1)
    (highlight-lines-matching-regexp "ERROR:" 'hi-red-b)
    (highlight-lines-matching-regexp "NOTE:" 'hi-blue-b)))

;; The condition in this function that checks for the =log= extension,
;; allows me to hook it to the loading of any file:
(add-hook 'find-file-hook 'ha/highlite-logs)

;; Turn on specific word groupings for specific occasions. We begin
;;    with highlighting keywords I use during note-taking sessions at
;;    the end of a sprint.
(defun ha/sprint-retrospective-highlighting ()
  "Highlights the good, the bad and the improvements to make when taking notes."
  (interactive)
  (hi-lock-mode t)
  (highlight-lines-matching-regexp "^   [-*] " 'hi-black-b)
  (highlight-phrase "TODO:?" 'hi-black-b)
  (highlight-regexp "(?Good)?:?" 'hi-green-b)
  (highlight-regexp "(?Bad)?:?" 'hi-red-b)
  (highlight-regexp "Imp\\(rove\\)?:" 'hi-blue-b))

;; This works really well with other commands, including
;;    [[https://github.com/Bruce-Connor/fancy-narrow][fancy-narrow]], where I can visually high-light a section of a
;;    buffer. Great for code-reviews and other presentations.
(use-package fancy-narrow
  :ensure t
  :config
  (defun ha/highlight-block ()
    "Highlights a 'block' in a buffer defined by the first blank
     line before and after the current cursor position. Uses the
          'fancy-narrow' mode to high-light the block."
    (interactive)
    (let (cur beg end)
      (setq cur (point))
      (setq end (or (re-search-forward  "^\s*$" nil t) (point-max)))
      (goto-char cur)
      (setq beg (or (re-search-backward "^\s*$" nil t) (point-min)))
      (fancy-narrow-to-region beg end)
      (goto-char cur)))

  (defun ha/highlight-section (num)
    "If some of the buffer is highlighted with the `fancy-narrow'
          mode, then un-highlight it by calling `fancy-widen'.

          If region is active, call `fancy-narrow-to-region'.

          If NUM is 0, highlight the current block (delimited by blank
          lines). If NUM is positive or negative, highlight that number
          of lines.  Otherwise, called `fancy-narrow-to-defun', to
          highlight current function."
    (interactive "p")
    (cond
     ((fancy-narrow-active-p)  (fancy-widen))
     ((region-active-p)        (fancy-narrow-to-region (region-beginning) (region-end)))
     ((= num 0)                (ha/highlight-block))
     ((= num 1)                (fancy-narrow-to-defun))
     (t                        (progn (ha/expand-region num)
                                      (fancy-narrow-to-region (region-beginning) (region-end))))))

  :bind ("C-M-+" . ha/highlight-section))

;; This nifty function from [[http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html][Endless Parenthesis]] is a nice replacement
;;    for many other narrowing keybindings that I use:
(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
     Intelligently means: region, subtree, or defun, whichever applies
     first.

     With prefix P, don't widen, just narrow even if buffer is already
     narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
        (t (narrow-to-defun))))
(global-set-key (kbd "C-x n x") 'narrow-or-widen-dwim)


;;; Jumping to Windows
;;

;; ace-window
(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?o))
  (global-set-key (kbd "C-x o") 'ace-window)
  :diminish ace-window-mode)

;;; Selecting a Buffer
;;
;; note: helm has alsow switching buffer plugin
(global-set-key (kbd "<f8>") 'ido-switch-buffer)
(global-set-key (kbd "S-<f8>") 'ibuffer)

(use-package kpm-list
  :ensure t
  :bind ("S-<f8>" . kpm-list)
  ("C-x C-b" . kpm-list))

;;; Controlling Window Size
;;

;; Often, while on my laptop, I want the current window to be ‘large
;;    enough for work’, and this is bound to =<f9> .= (period). ;; NOTE: does not work!!

(global-unset-key (kbd "C-c w"))
(global-set-key (kbd "C-c w r") 'ha/window-standard-size)

;; If I've enlarged the window, I can restore that window to its
;;    original size, so this requires a /buffer local variable/:

(make-variable-buffer-local 'window-width-original)

;; Now a function that either changes the width to 80, or back to the
;;    original size if already at 80.

(defun ha/window-standard-size (arg)
  "Sets the size of the current window to 80 characters, unless
     it already is 80 characters, in which case, set it back to its
     previous size. A prefix ARG can be given to set the window to a
     particular width."
  (interactive "p")

  ;; If not already set, let's store the current window width in our
  ;; buffer-local variable.
  (if (not (local-variable-p 'window-width-original))
      (setq window-width-original (window-width)))

  ;; The 'goal' is 80 unless we get a better argument, C-u 60 ...
  (let* ((goal-width (if (> arg 8) arg 80))
         (new-width (- goal-width (window-width))))

    (if (= new-width 0)    ; Already enlarged? Restore:
        (enlarge-window-horizontally (- window-width-original goal-width))
      (enlarge-window-horizontally new-width))))


;;; Controlling Window Placement
;;

;; Change window configuration and then return to the old
;;    configuration with [[http://www.emacswiki.org/emacs/WinnerMode][winner-mode]].  Use =Control-C Arrow= keys to
;;    cycle through window/frame configurations.
(use-package winner
  :ensure t
  :init (winner-mode 1))

;; While =winner-mode= is easy to keep the current window configuration
;;    /clean/, the [[https://github.com/wasamasa/eyebrowse][eyebrowse]] project seems to have a good approach to
;;    managing multiple configurations.
(use-package eyebrowse
  :ensure t
  :init   (eyebrowse-mode t))


;;; Better Jumping
;;

;; Mostly using the [[https://github.com/abo-abo/avy][avy]] project's [[help:avy-goto-word-1][avy-goto-word-1]] function, so I bind
;;    that to =C-c j=, but the recent update to include a timer feature,
;;    seems awful sweet:
;; Other options (that require more of my memory), are bound
;;    to =C-c k=.
(use-package avy
  :ensure t
  :commands avy-goto-word-1 avy-goto-char-1 avy-goto-line avy-goto-char-timer
  :bind
  ("C-c j"   . avy-goto-word-1)
  ("C-c k k" . avy-goto-char-timer)
  ("C-c k j" . avy-goto-word-1)
  ("C-c k c" . avy-goto-char-1)
  ("C-c k l" . avy-goto-line))


;;; Unfill Paragraph
;;

;; Unfilling a paragraph joins all the lines in a paragraph into a
;;    single line. Taken from [[http://www.emacswiki.org/UnfillParagraph][here]].
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(define-key global-map "\M-Q" 'unfill-paragraph)

;;; General Behavior Fixes
;;

;; The subtle changes I've been making to Emacs behavior has grown
;;    until I felt I should move it into [[file:emacs-fixes.org][its
;;    own source file]].
(require 'init-fixes)


;;; Multiple Cursors
;;

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key
   (kbd "C-c C-.")
   (defhydra hydra-multiple-cursors ()
     "multiple-cursors"
     ("." mc/mark-all-dwim                   "all-dwim")
     ("C-." mc/mark-all-like-this-dwim       "all-like-dwim")
     ("n" mc/mark-next-like-this             "next")
     ("p" mc/mark-previous-like-this         "previous")
     ("a" mc/mark-all-like-this              "mark-all")
     ("N" mc/mark-next-symbol-like-this      "next-symbol")
     ("P" mc/mark-previous-symbol-like-this  "previous-symbol")
     ("A" mc/mark-all-symbols-like-this      "all-symbols")
     ("f" mc/mark-all-like-this-in-defun     "in-func")
     ("l" mc/edit-lines                      "all-lines")
     ("e" mc/edit-ends-of-lines              "end-lines"))))


;;; Expand Region
;;

;; Wherever you are in a file, and whatever the type of file, you can
;;    slowly increase a region selection by logical segments by using
;;    Magnar's [[https://github.com/magnars/expand-region.el][expand-region]] project.

;;    However, the normal experience for =expand-region= is interactive,
;;    expected to be called repeatedly to expand and contract the regions
;;    based on syntax, and whatnot. Since I am seldom sure what I will
;;    select if I give this function a numeric prefix, I created a
;;    wrapper function that will (when given a number), just select the
;;    number of lines for the region. Select the current line with a 0
;;    argument. No argument (well, =lines= is given 1 with no argument),
;;    then it just calls =expand-region=:
(use-package expand-region
  :ensure t
  :config
  (defun ha/expand-region (lines)
    "Prefix-oriented wrapper around Magnar's `er/expand-region'.

     Call with LINES equal to 1 (given no prefix), it expands the
     region as normal.  When LINES given a positive number, selects
     the current line and number of lines specified.  When LINES is a
     negative number, selects the current line and the previous lines
     specified.  Select the current line if the LINES prefix is zero."
    (interactive "p")
    (cond ((= lines 1)   (er/expand-region 1))
          ((< lines 0)   (ha/expand-previous-line-as-region lines))
          (t             (ha/expand-next-line-as-region (1+ lines)))))

  (defun ha/expand-next-line-as-region (lines)
    (message "lines = %d" lines)
    (beginning-of-line)
    (set-mark (point))
    (end-of-line lines))

  (defun ha/expand-previous-line-as-region (lines)
    (end-of-line)
    (set-mark (point))
    (beginning-of-line (1+ lines)))

  :bind ("C-=" . ha/expand-region))

;;; Block Wrappers
;;
;; While the =M-(= binding to =insert-pair= is great, I often need to
;; wrap with other characters:
(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-{") 'insert-pair)
;;(global-set-key (kbd "M-<") 'insert-pair) ;; conflict with go to start
(global-set-key (kbd "M-'") 'insert-pair)
(global-set-key (kbd "M-`") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)


;; But [[https://github.com/rejeep/wrap-region.el][wrap-region]] is even more flexible. In most editors, selecting
;;    text and typing anything replaces the selected text (see the
;;    [[info:emacs#Using%20Region][delete-selection-mode]]), but in this case, we can do something
;;    different... like wrapping:
(use-package wrap-region
  :ensure   t
  :config
  (wrap-region-global-mode t)
  (wrap-region-add-wrappers
   '(("(" ")")
     ("[" "]")
     ("{" "}")
     ("<" ">")
     ("'" "'")
     ("\"" "\"")
     ("‘" "’"   "q")
     ("“" "”"   "Q")
     ("*" "*"   "b"   org-mode)                 ; bolden
     ("*" "*"   "*"   org-mode)                 ; bolden
     ("/" "/"   "i"   org-mode)                 ; italics
     ("/" "/"   "/"   org-mode)                 ; italics
     ("~" "~"   "c"   org-mode)                 ; code
     ("~" "~"   "~"   org-mode)                 ; code
     ("=" "="   "v"   org-mode)                 ; verbatim
     ("=" "="   "="   org-mode)                 ; verbatim
     ("_" "_"   "u" '(org-mode markdown-mode))  ; underline
     ("**" "**" "b"   markdown-mode)            ; bolden
     ("*" "*"   "i"   markdown-mode)            ; italics
     ("`" "`"   "c" '(markdown-mode ruby-mode)) ; code
     ("`" "'"   "c"   lisp-mode)                ; code
     ))
  :diminish wrap-region-mode)

;; But in order to wrap text in a more general way (with just about
;;    any textual string), we need something more. Especially with the
;;    =expand-region= command, wrapping a logical block of text with a
;;    beginning and ending string really makes sense.
(defun surround (start end txt)
  "Wraps the specified region (or the current 'symbol / word'
     with some textual markers that this function requests from the
     user. Opening-type text, like parens and angle-brackets will
     insert the matching closing symbol.

     This function also supports some org-mode wrappers:

       - `#s` wraps the region in a source code block
       - `#e` wraps it in an example block
       - `#q` wraps it in an quote block"
  (interactive "r\nsEnter text to surround: " start end txt)

  ;; If the region is not active, we use the 'thing-at-point' function
  ;; to get a "symbol" (often a variable or a single word in text),
  ;; and use that as our region.

  (if (not (region-active-p))
      (let ((new-region (bounds-of-thing-at-point 'symbol)))
        (setq start (car new-region))
        (setq end (cdr new-region))))

  ;; We create a table of "odd balls" where the front and the end are
  ;; not the same string.
  (let* ((s-table '(("#e" . ("#+BEGIN_EXAMPLE\n" "\n#+END_EXAMPLE") )
                    ("#s" . ("#+BEGIN_SRC \n"    "\n#+END_SRC") )
                    ("#q" . ("#+BEGIN_QUOTE\n"   "\n#+END_QUOTE"))
                    ("<"  . ("<" ">"))
                    ("("  . ("(" ")"))
                    ("{"  . ("{" "}"))
                    ("["  . ("[" "]"))))    ; Why yes, we'll add more
         (s-pair (assoc-default txt s-table)))

    ;; If txt doesn't match a table entry, then the pair will just be
    ;; the text for both the front and the back...
    (unless s-pair
      (setq s-pair (list txt txt)))

    (save-excursion
      (narrow-to-region start end)
      (goto-char (point-min))
      (insert (car s-pair))
      (goto-char (point-max))
      (insert (cadr s-pair))
      (widen))))

(global-set-key (kbd "C-+") 'surround)

;; To make it easier to call from other functions, let's wrap that
;;    wrapper:
(defun surround-text (txt)
  (if (region-active-p)
      (surround (region-beginning) (region-end) txt)
    (surround nil nil txt)))

;; This function returns an interactive lambda expression, suitable
;;    for adding to a key-binding:
(defun surround-text-with (surr-str)
  "Returns an interactive function that when called, will surround the region (or word) with the SURR-STR string."
  (lexical-let ((text surr-str))
    (lambda ()
      (interactive)
      (surround-text text))))

;;; -------------------------------------------------------------------------
;;; LOADING AND FINDING FILES
;;; -------------------------------------------------------------------------

;;; Projectile
;;
;; The [[https://github.com/bbatsov/projectile][Projectile]] project is a nifty way to run commands and search
;;    for files in a particular "project". Its necessity is less now that
;;    IDO with flexible matching seems to always just find what I need.
;;
(use-package projectile
  :load-path "site-lisp/projectile"
  :ensure t
  :init (projectile-global-mode 1)
  :commands projectile-ag
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (use-package helm-projectile
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")))
;; not sure ...
(use-package helm-projectile
  :defer t :ensure t
  :ensure helm-projectile)

;;; Dired Options
;;
;;Between =M-!= and starting [[Eshell][Eshell]], comes =dired= (=C-x
;;d=).
(setq ls-lisp-use-insert-directory-program nil)

;; This enhancement to dired hides the ugly details until you hit
;;    '(' and shows the details with ')'. I also change the [...] to a
;;    simple asterisk.
(use-package dired-details
       :ensure t
       :init   (setq dired-details-hidden-string "* ")
       :config (dired-details-install))

;; The ability to create a dired buffer based on searching for files
;;    in a directory tree with =find-name-dired= is fantastic. The
;;    [[http://www.masteringemacs.org/articles/2011/03/25/working-multiple-files-dired/][following magic]] optimizes this approach:
(use-package find-dired
        :ensure t
        :init (setq find-ls-option '("-print0 | xargs -0 ls -od" . "-od")))
;; The [[http://www.masteringemacs.org/articles/2014/04/10/dired-shell-commands-find-xargs-replacement/][dired-x project]] seems useful:
(require 'dired-x)

;;; Tramp
;;

;; The ability to edit files on remote systems is a wonderful win,
;;    since it means I don't need to have my Emacs environment running on
;;    remote machines (still a possibility, just not a requirement).

;;    According to [[http://www.gnu.org/software/emacs/manual/html_node/tramp/Filename-Syntax.html][the manual]], I can access a file over SSH, via:

;;    #+BEGIN_EXAMPLE
;;    /ssh:10.52.224.67:blah
;;    #+END_EXAMPLE

;;    If I set the default method to SSH, I can do this:

;;    #+BEGIN_EXAMPLE
;;    /10.52.224.67:blah
;;    #+END_EXAMPLE

;;    So, let's do it...

(setq tramp-default-method "ssh")

;;; Editing Root Files
;;

;; According to [[http://emacs-fu.blogspot.com/2013/03/editing-with-root-privileges-once-more.html][Emacs Fu]], we can use the wonderful Tramp to edit
;;    Root-owned files, as in:

(defun ha/find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
     root-privileges (using tramp/sudo), if the file is not writable by
     user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

;; The trick, as always, is finding the correct keybinding... but I
;;    have the =C-c f= as prefix for loading all sorts of files...

(global-set-key (kbd "C-c f r") 'ha/find-file-as-root)

;;; Helm
;;

(use-package helm
  :ensure t
  :init (load "init-helm-nagi.el"))

;;; Grep for my Notes
;;

;; on CLI
;; install on linux
;; sudo apt-get install silversearcher-ag
;; install on mac
;;   brew tap homebrew/dupes
;;   brew install homebrew/dupes/grep
;;
;; on Emacs
;; Silver searcher
;;
;; - =ag-project-at-point= :: sets the query with the word at point, use: =C-c p s s=
;; - =ag-regexp= :: searches for regular expressions in a chosen
;;                  directory (*Note:* the =ag= command prompts with
;;                  =regexp=, but it adds a =--literal= option to the command)
;; - =C-u= :: Adding a prefix adds command line options, like =-s= or
;;            =-i= to specify case-sensitivity.

;;Create collection of [[file:~/.agignore][ignorable files]] so it doesn’t look in backup files:
;; #+BEGIN_SRC org :tangle ~/.agignore
;;   #.*
;; #+END_SRC
(use-package ag
  :ensure    t
  :commands  ag
  :init      (setq ag-highlight-search t)
  :config    (add-to-list 'ag-arguments "--word-regexp"))

(use-package helm-ag
  :ensure    t)


;;; Recent files list
;;

;; Emacs already has the recent file
;; listing available, just not turned on.
;; We do not want to stat all the files when Emacs starts up because
;;    files read by Tramp will slow down the start time.
(use-package recentf
  :init
  (setq recentf-max-menu-items 25)
  (setq recentf-auto-cleanup 'never)
  ;; (setq recentf-keep '(file-remote-p file-readable-p))
  (recentf-mode 1)
  :bind ("C-c f f" . recentf-open-files))

;;; Backup Settings
;;

;; this is from old
;;
(setq backup-directory-alist `(("." . ,(expand-file-name
                 (na/emacs-subdirectory "backups"))))
      backup-by-copying t
      version-control t
      kept-old-versions 2
      kept-new-versions 20
      delete-old-versions t)
(setq tramp-backup-directory-alist backup-directory-alist)

(defun save-all ()
  "Saves all dirty buffers without asking for confirmation."
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'save-all)

;;; -------------------------------------------------------------------------
;;; WORD SMITHING
;;; -------------------------------------------------------------------------

;;; Auto Insertion
;;

;; Just beginning to get a collection of templates to automatically
;;    insert if a blank file is loaded.
(use-package autoinsert
  :init
  (setq auto-insert-directory (na/emacs-subdirectory "templates/"))
  ;; Don't want to be prompted before insertion:
  (setq auto-insert-query nil)

  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1))

;;Add a =:config= section to configure static insertion, and add:
(define-auto-insert "\\.html?$" "default-html.html")

;; However, auto insertion requires entering data for particular fields,
;;    and for that Yasnippet is better, so in this case, we combine them:
(defun ha/autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

;;Now bind many of the templates for auto-insert and field expansion:
(use-package autoinsert
  :config
  (define-auto-insert "\\.el$" ["default-lisp.el" ha/autoinsert-yas-expand])
  (define-auto-insert "\\.sh$" ["default-sh.sh" ha/autoinsert-yas-expand])
  (define-auto-insert "/bin/"  ["default-sh.sh" ha/autoinsert-yas-expand])
  (define-auto-insert "\\.html?$" ["default-html.html"
                                   ha/autoinsert-yas-expand]))

;;; Auto complete
;;

;; Using [[http://company-mode.github.io/][company-mode]] for all my auto completion needs.

;;    Like [[https://github.com/vspinu/company-math][this idea]] of being able to easily insert math
;;    symbols based on LaTeX keywords. Start typing a backslash.
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  :diminish company-mode)

;; Take advantage of idle time by displaying some documentation
;;    using
;;    [[https://www.github.com/expez/company-quickhelp][company-quickhelp]]
;;    project.
;; This also requires [[https://github.com/pitkali/pos-tip/blob/master/pos-tip.el][pos-tip]].
(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1))


;;; Yasnippets
;;

;; The [[https://github.com/capitaomorte/yasnippet][yasnippet project]] allows me to create snippets of code that
;;    can be brought into a file, based on the language.
;; *Note:*: the =snippets= directory contains directories for each
;;    mode, e.g.  =clojure-mode= and =org-mode=.
;; i used org-mode from https://github.com/yyr/yasnippets-orgmode.git
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (na/emacs-subdirectory "snippets")))



;;; Spelling Correction with Abbreviation Mode
;;
;; TODO: TODO:

;;; Spell Checking
;;
;; I like spell checking with [[http://www.emacswiki.org/emacs/FlySpell][FlySpell]], which uses the built-in
;;    spell-check settings of [[https://www.gnu.org/software/ispell/][ispell]].

;;    The [[http://aspell.net][ASpell]] project is better supported than ISpell.

;;Start for all text modes (but not for log files):
(use-package flyspell
  :ensure t
  :diminish flyspell-mode
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)

  (dolist (hook '(text-mode-hook org-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))

  (dolist (hook '(change-log-mode-hook log-edit-mode-hook org-agenda-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))

  :config
  (setq ispell-program-name "/usr/bin/aspell"
        ispell-dictionary "american" ; better for aspell
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")
        ispell-list-command "--list")

  (add-to-list 'ispell-local-dictionary-alist '(nil
                                                "[[:alpha:]]"
                                                "[^[:alpha:]]"
                                                "['‘’]"
                                                t
                                                ("-d" "en_US")
                                                nil
                                                utf-8)))

;; ASpell automatically configures a personal dictionary
;;    at =~/.aspell.en.pws=, so no need to configure that.




;;; -------------------------------------------------------------------------
;;; MISCELLANEOUS SETTINGS
;;; -------------------------------------------------------------------------

;;; Line Numbers
;;

;; Turn =linum-mode= on/off with =Command-K= (see the [[*Macintosh][Macintosh]]
;;    section above).  However, I turn this on automatically for
;;    programming modes.
(add-hook 'prog-mode-hook 'linum-mode)

;; If we make the line numbers a fixed size, then increasing or
;;    decreasing the font size doesn't truncate the numbers:
(defun fix-linum-size ()
  (interactive)
  (set-face-attribute 'linum nil :height 110))
(add-hook 'linum-mode-hook 'fix-linum-size)

;; If we alternate between line numbers and no-line numbers, I also
;;    have to turn on/off the fringe. Actually, this is really only
;;    useful when giving presentations.
(defun linum-off-mode ()
  "Toggles the line numbers as well as the fringe. This allows me
     to maximize the screen estate."
  (interactive)
  (if linum-mode
      (progn
        (fringe-mode '(0 . 0))
        (linum-mode -1))
    (fringe-mode '(8 . 0))
    (linum-mode 1)))

(global-set-key (kbd "A-C-K") 'linum-off-mode)
(global-set-key (kbd "C-c T k") 'linum-off-mode)  ;; For Linux

;; I'm intrigued with the [[https://github.com/coldnew/linum-relative][linum-relative]] mode (especially since I can
;;    toggle between them). The idea is that I can see the line that I
;;    want to jump to (like one 9 lines away), and then =C-9 C-n= to
;;    quickly pop to it.

(use-package linum-relative
  :ensure t
  :config
  ;; Otherwise, let's take advantage of the relative line numbering:
  (defun linum-new-mode ()
    "If line numbers aren't displayed, then display them.
          Otherwise, toggle between absolute and relative numbers."
    (interactive)
    (if linum-mode
        (linum-relative-toggle)
      (linum-mode 1)))

  :bind ("A-k" . linum-new-mode)
  ("C-c T K" . linum-new-mode))   ;; For Linux


;;; Breadcrumbs
;;

;; I often flubber my attempts at walking back through the movements
;;    with those two key sequences. Better to set this variable so that
;;    repeated =C-SPC= continue to pop back through the ring:
(setq set-mark-command-repeat-pop t)

;; More than the breadcrumbs left by marking, the [[http://www.emacswiki.org/emacs/GotoChg][goto-chg]] project
;;    let's me walk back to where I last edited, which is usually more
;;    accurate:
(use-package goto-chg
  :ensure t
  :bind (("M-p" . goto-last-change)
         ("M-n" . goto-last-change-reverse)))

;; Use =C-u 0 M-p= shows a description of the change you made at each point.

;;    However, if walking back through your /historical trail/ crosses
;;    files, then dropping some phat marks is the correct approach.

;;    Leave a mark every time I re-center the screen. Then, walk back and
;;    forth through its history (using the [[https://github.com/pheaver/breadcrumb][breadcrumb]] project):
(use-package breadcrumb
  :load-path "~/Other/breadcrumb/"
  :commands bc-set bc-previous bc-next
  :init (defun ha/mark-and-center ()
          "Recenter the display and drops a breadcrumb."
          (interactive)
          (bc-set)
          (recenter-top-bottom))
  :config (unbind-key "A-." mac-key-mode-map)
  :bind (("C-l" . ha/mark-and-center)
         ("A-," . bc-previous)
         ("A-." . bc-next)))


;;; Smart Scan
;;

;; Use the =M-n= to search the buffer for the word the cursor is
;;    currently pointing. =M-p= to go backwards. See [[http://www.masteringemacs.org/articles/2011/01/14/effective-editing-movement/][this essay]] for
;;    details.

(use-package smartscan
  :ensure t
  :bind ("M-n" . smartscan-symbol-go-forward)
  ("M-p" . smartscan-symbol-go-backward))


;;; Strip Whitespace on Savev
;;

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Save File Position
;;

;; Save the point position for every file, and restore it when that
;;   file is reloaded.
(require 'saveplace)
(setq-default save-place t)
(setq save-place-forget-unreadable-files t)
(setq save-place-skip-check-regexp
      "\\`/\\(?:cdrom\\|floppy\\|mnt\\|/[0-9]\\|\\(?:[^@/:]*@\\)?[^@/:]*[^@/:.]:\\)")


;;; Better Searching and Visual Regular Expressions
;;
;; replaced with Helm, so C-s works like a charm


;;; Flycheck
;;

;; [[https://github.com/flycheck/flycheck][Flycheck]] seems to be quite superior to good ol' Flymake.
(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))


;;; Hungry Delete
;;

;; The Hungry Delete project is a [[http://endlessparentheses.com//hungry-delete-mode.html][free feature]], where deleting any
;;    space, deletes ALL spaces.

;;    This is already built into Emacs with the following:
;;    - =M-\= :: Removes all spaces
;;    - =M-SPC= :: Removes extra spaces, leaving just one
;;    - =M-^= :: Joins current line with previous line (doesn't matter
;;         where the point is on the line)
;;    - =M-- M-1 M-SPC= :: Joins next line to this one (if point at end
;;         of line) separated by a space ... quite the chording, eh?


;;; -------------------------------------------------------------------------
;;; PROGRAMMING LANGUAGES
;;; -------------------------------------------------------------------------








;;------------------------------------------------------- >>>>>>>

;; setting PATH for eshell
(setenv "PATH" (concat
                "/home/nagi/opt/browser/firefox" ":"
                (getenv "PATH")))



;; case-sensitivity
;; nil means case-sensitive; non-nil means case-insensitive
(setq case-fold-search t)

;; Add-newline-at-end-of-files
(setq require-final-newline t)

;; y-or-n-p
(fset 'yes-or-no-p 'y-or-n-p)

;; Affiche-l'heure-au-format-24h
;; (setq display-time-24hr-format t)
;; (setq display-time-day-and-date t)
;; (display-time)
;; (setq display-time-use-mail-icon t)

;; Limite-max-lisp
(setq max-lisp-eval-depth 40000)
(setq max-specpdl-size 100000)

;; Increase GC
(setq gc-cons-threshold 20000000)

;; Annoyances section
;;
(global-set-key (kbd "<f11>") nil)
(add-hook 'emacs-startup-hook #'(lambda ()
                                  (when (get-buffer "*Compile-Log*")
                                    (kill-buffer "*Compile-Log*")
                                    (delete-other-windows))))

;; Disable uniquify enabled by default in 24.4.
(setq uniquify-buffer-name-style nil)

;; electric-indent-mode
(electric-indent-mode -1)

(setq register-preview-delay nil)

;; No-startup-screen
(setq inhibit-startup-message t)

;; consequent-log-file
(setq message-log-max 1000)

;; kill-ring
(setq kill-ring-max 60)

;; mark ring
(setq mark-ring-max 60)



(setq font-lock-maximum-decoration t)

;; column-number in mode-line.
(column-number-mode 1)

;; Environment variables
;;
;; grep matches with background yellow and foreground black
(setenv "GREP_COLORS" "ms=30;43:mc=30;43:sl=01;37:cx=:fn=35:ln=32:bn=32:se=36")

;; For eshell env settings.
(setenv "STARDICT_DATA_DIR" "~/.stardict/dic")
(prefer-coding-system 'utf-8)

;; Save-minibuffer-history
(setq savehist-file "~/.emacs.d/history"
      history-delete-duplicates t)
(setq history-length 100) ; default is 30.
(savehist-mode 1)

;; Themes
;;(defvar nagi-theme-directory "~/.emacs.d/themes/naquadah-theme")
;;(unless (< emacs-major-version 24)
;;  (setq custom-theme-directory nagi-theme-directory))

;; Load my favourite theme.
;; under ~/.emacs.d/themes, git https://github.com/jd/naquadah-theme.git
;;(add-hook 'emacs-startup-hook #'(lambda () (load-theme 'naquadah)))


;;; Frame and window config.
;;
;;
;; My current-font: [EVAL]: (assoc-default 'font (frame-parameters))
;; Choose a font:   [EVAL]: (progn (when (require 'helm-font) (helm 'helm-source-xfonts)))
;; Choose a color:  [EVAL]: (progn (when (require 'helm-color) (helm 'helm-source-colors)))
;; To reload .Xresources [EVAL]: (shell-command xrdb "~/.Xresources")

;; (defvar tv-default-font (assoc-default 'font (frame-parameters)))
;; (setq-default frame-background-mode 'dark)
;; (setq initial-frame-alist '((fullscreen . maximized)))
;; (setq frame-auto-hide-function 'delete-frame)


;; (if (or (daemonp)
;;         (not (window-system))
;;         (< emacs-major-version 24))
;;     (setq default-frame-alist `((vertical-scroll-bars . nil)
;;                                 (tool-bar-lines . 0)
;;                                 (menu-bar-lines . 0)
;;                                 (title . ,(format "Emacs-%s" emacs-version))
;;                                 (font . "-unknown-DejaVu Sans Mono-bold-normal-normal-*-14-*-*-*-m-0-iso10646-1")
;;                                 (cursor-color . "red")))

;;     (setq default-frame-alist `((foreground-color . "Wheat")
;;                                 (background-color . "black")
;;                                 (alpha . 90)
;;                                 ;; New frames go in right corner.
;;                                 (left . ,(- (* (window-width) 8) 160)) ; Chars are 8 bits long.
;;                                 (vertical-scroll-bars . nil)
;;                                 (title . ,(format "Emacs-%s" emacs-version))
;;                                 (tool-bar-lines . 0)
;;                                 (menu-bar-lines . 0)
;;                                 (font . ,tv-default-font)
;;                                 (cursor-color . "red")
;;                                 (fullscreen . nil)
;;                                 )))

;; Speedbar
;; (add-hook 'speedbar-load-hook
;;           #'(lambda ()
;;               (setq speedbar-frame-parameters
;;                     `((minibuffer . nil)
;;                       (font . ,tv-default-font)
;;                       (width . 20)
;;                       (fullscreen . nil) ; Not needed when fullscreen isn't set in .Xressources.
;;                       (left . ,(- (* (window-width) 8)
;;                                   (frame-width))) ; Speed-bar on right of screen.
;;                       (border-width . 0)
;;                       (menu-bar-lines . 0)
;;                       (tool-bar-lines . 0)
;;                       (unsplittable . t)
;;                       (left-fringe . 0)))))

;;; Special buffer display.
;;
;;
;; (setq special-display-regexps `(("\\*Help"
;;                                  (minibuffer . nil)
;;                                  (width . 80)
;;                                  (height . 24)
;;                                  (left-fringe . 0)
;;                                  (border-width . 0)
;;                                  (menu-bar-lines . 0)
;;                                  (tool-bar-lines . 0)
;;                                  (unsplittable . t)
;;                                  (top . 24)
;;                                  (left . 450)
;;                                  (background-color . "Lightsteelblue1")
;;                                  (foreground-color . "black")
;;                                  (alpha . nil)
;;                                  (fullscreen . nil))
;;                                 ("\\*Compile-Log"
;;                                  (minibuffer . nil)
;;                                  (width . 85)
;;                                  (height . 24)
;;                                  (left-fringe . 0)
;;                                  (border-width . 0)
;;                                  (menu-bar-lines . 0)
;;                                  (tool-bar-lines . 0)
;;                                  (unsplittable . t)
;;                                  (top . 24)
;;                                  (left . 450)
;;                                  (background-color . "Brown4")
;;                                  (foreground-color . "black")
;;                                  (alpha . nil)
;;                                  (fullscreen . nil))
;;                                 ("\\*Dict"
;;                                  (minibuffer . nil)
;;                                  (width . 80)
;;                                  (height . 24)
;;                                  (left-fringe . 0)
;;                                  (border-width . 0)
;;                                  (menu-bar-lines . 0)
;;                                  (tool-bar-lines . 0)
;;                                  (unsplittable . t)
;;                                  (top . 24)
;;                                  (left . 450)
;;                                  (background-color . "LightSteelBlue")
;;                                  (foreground-color . "DarkGoldenrod")
;;                                  (alpha . nil)
;;                                  (fullscreen . nil))
;;                                 ))

;; Don't split this windows horizontally
(setq split-width-threshold nil)

;; Pas-de-dialog-gtk
(setq use-file-dialog nil)

;;; Powerline
;; testing
;; (use-package powerline
;;        :ensure t)
;; (custom-set-faces
;;      '(mode-line-buffer-id ((t (:foreground "#000000" :bold t))))
;;      '(which-func ((t (:foreground "#77aaff"))))
;;      '(mode-line ((t (:foreground "#000000" :background "#dddddd" :box nil))))
;;      '(mode-line-inactive ((t (:foreground "#000000" :background "#bbbbbb" :box nil)))))



;;; Ediff
;;
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)


;; show-paren-mode
;;
(show-paren-mode 1)
(setq show-paren-ring-bell-on-mismatch t)

;; Start-emacs-server
;;
;; (add-hook 'after-init-hook #'(lambda ()
;;                                (unless (daemonp)
;;                                  (server-start)
;;                                  (setq server-raise-frame t))))

;; Path-to-abbrev-file
;;(setq abbrev-file-name "/home/thierry/.emacs.d/.abbrev_defs")

;; Copy/paste
(setq select-active-regions t)
(setq x-select-enable-clipboard-manager nil)

;; Enable-commands-disabled-by-default
(put 'narrow-to-region 'disabled nil) ; C-x n n
(put 'narrow-to-page 'disabled nil)   ; C-x n p
(put 'scroll-left 'disabled nil)     ; C-x > or <
(put 'downcase-region 'disabled nil) ; C-x C-l
(put 'upcase-region 'disabled nil)   ; C-x C-u
(put 'set-goal-column 'disabled nil) ; C-x C-n ==> disable with C-u
(put 'dired-find-alternate-file 'disabled nil) ; a in dired

;; setup-minibuffer
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; Woman/man
(setq woman-use-own-frame nil)
(setq Man-notify-method 'pushy)
(defface man-args-face '((t (:foreground "Magenta" :underline t)))
  "*Face used in man page to show arguments and sections."
  :group 'man)

;; Printing
;; (setq lpr-command "gtklp")
;; (setq printer-name "EpsonStylus")
;; (setq-default ps-print-header nil)
;; (setq ps-font-size   '(10 . 11.5))
;; (setq ps-font-family 'Courier)

;; auto-compression-mode
(auto-compression-mode 1)

;; Mode-lecture-photo-auto
(auto-image-file-mode 1)

;; Allow scrolling horizontally in large images
(add-hook 'image-mode-hook #'(lambda () (set (make-variable-buffer-local 'auto-hscroll-mode) nil)))

;; line-move-visual.
(setq line-move-visual nil)

;; Trash
;; (setq delete-by-moving-to-trash t)

;; Minibuffers completion
(setq completion-cycle-threshold t) ; always cycle, no completion buffers.

;; Diff
(customize-set-variable 'diff-switches "-w")

;; Report bug
(setq report-emacs-bug-no-explanations t)



;; Prompt shell read only
(setq comint-prompt-read-only t)

;; Newline and indent in `sh-mode'.
(add-hook 'sh-mode-hook #'(lambda ()
                            (define-key sh-mode-map (kbd "RET") 'newline-and-indent)))

;; winner-mode config
(setq winner-boring-buffers '("*Completions*"
                              "*Compile-Log*"
                              "*inferior-lisp*"
                              "*Fuzzy Completions*"
                              "*Apropos*"
                              "*Help*"
                              "*cvs*"
                              "*Buffer List*"
                              "*Ibuffer*"
                              ))

(winner-mode 1)

;; Display time in mode-line
;; (setq display-time-string-forms
;;       '( ;; date
;;         (if (and (not display-time-format) display-time-day-and-date)
;;             (format-time-string "[%a %e %b " now)
;;             "")
;;         ;; time
;;         (concat
;;          (propertize
;;           (format-time-string (or display-time-format
;;                                   (if display-time-24hr-format " %H:%M" " %-I:%M%p"))
;;                               now)
;;           'face '((:foreground "green"))
;;           'help-echo (format-time-string " %a %b %e, %Y" now))
;;          (and time-zone " (") time-zone (and time-zone ")")
;;          "]")
;;         ;; cpu load average
;;         ;; (if (and load (not (string= load "")))
;;         ;;     (format "cpu:%s" load) "")
;;         ""
;;         ;; mail
;;         ""))

;; Mode-line
;(set-face-attribute 'mode-line-emphasis nil :foreground "red")

;; World-time
;; (add-to-list 'display-time-world-list '("Greenwich" "Greenwich"))
;; (add-to-list 'display-time-world-list '("Australia/Sydney" "Sydney"))
;; (add-to-list 'display-time-world-list '("Australia/Melbourne" "Melbourne"))
;; (add-to-list 'display-time-world-list '("Australia/Canberra" "Canberra"))
;; (add-to-list 'display-time-world-list '("America/Chicago" "Chicago"))
;; (add-to-list 'display-time-world-list '("America/Denver" "Denver"))
;; (add-to-list 'display-time-world-list '("America/Los_Angeles" "Los_Angeles/Seattle"))
;; (add-to-list 'display-time-world-list '("America/Denver" "Moab"))
;; (add-to-list 'display-time-world-list '("America/Vancouver" "Vancouver"))
;; (add-to-list 'display-time-world-list '("America/Montreal" "Montreal"))
;; (add-to-list 'display-time-world-list '("America/New_York" "Ottawa"))
;; (add-to-list 'display-time-world-list '("Europe/Moscow" "Moscow"))
;; (add-to-list 'display-time-world-list '("Europe/Berlin" "Berlin"))
;; (add-to-list 'display-time-world-list '("Europe/Oslo" "Oslo"))
;; (add-to-list 'display-time-world-list '("Europe/Lisbon" "Lisbon"))
;; (add-to-list 'display-time-world-list '("Asia/Dubai" "Dubai"))
;; (add-to-list 'display-time-world-list '("Asia/Tokyo" "Tokyo"))
;; (add-to-list 'display-time-world-list '("Hongkong" "Hongkong"))
;; (add-to-list 'display-time-world-list '("Indian/Antananarivo" "Antananarivo"))
;; (add-to-list 'display-time-world-list '("Indian/Reunion" "Reunion"))


;; flyspell-aspell
;; (setq-default ispell-program-name "aspell")
;; (setq ispell-local-dictionary "francais")

;; Kill buffer after C-d in ansi-term.
(defadvice term-sentinel (after kill-buffer activate)
  (kill-buffer))

;; Require with messages to debug more easily.
;; (defun tv-require (feature &optional filename noerror)
;;   (message "Loading %s..." (symbol-name feature))
;;   (condition-case err
;;       (if (require feature filename noerror)
;;           (message "Loading %s done" (symbol-name feature))
;;           (message "Loading %s Failed" (symbol-name feature)))
;;     (error
;;      (signal 'error (list feature (car err) (cadr err))))))

;;; load-path
;;
;; (dolist (i '("/usr/local/share/emacs/site-lisp"
;;              "/usr/local/share/emacs/site-lisp/mu4e"
;; 	     "~/elisp/"
;;              "~/elisp/google-maps.el"
;;              "~/elisp/Emacs-wgrep"
;;              "~/elisp/auctex"
;;              "~/elisp/auctex/preview"
;; 	     "~/elisp/autoconf-mode"
;; 	     "~/elisp/desktop-file-utils"
;; 	     "~/elisp/emacs-wget"
;; 	     "~/elisp/tex-utils"
;; 	     "~/elisp/ledger/"
;;              "~/elisp/helm"
;;              "~/elisp/helm-extensions"
;;              "~/.emacs.d/themes/"
;; 	     "~/.emacs.d/emacs-config/"
;; 	     ))
;;   ;; Add all at end of `load-path' to avoid conflicts.
;;   (add-to-list 'load-path (file-name-as-directory i) t))

;;; Async
;;
(use-package async
  :ensure t
  :config
  (progn
    (defun tv/async-byte-compile-file (file)
      (interactive "fFile: ")
      (let ((proc
             (async-start
              `(lambda ()
                 (require 'bytecomp)
                 ,(async-inject-variables "\\`load-path\\'")
                 (let ((default-directory ,(file-name-directory file)))
                   (add-to-list 'load-path default-directory)
                   (byte-compile-file ,file))))))

        (unless (condition-case err
                    (async-get proc)
                  (error (ignore (message "Error: %s" (car err)))))
          (message "Recompiling %s...FAILED" file))))))

(use-package dired-async
  :config (dired-async-mode 1))

(use-package async-bytecomp
  :config (setq async-bytecomp-allowed-packages '(all)))



;; Zoom-window
;;
(use-package zoom-window
  :ensure t
  :init (setq zoom-window-mode-line-color "DarkGreen")
  :bind ("C-x C-z" . zoom-window-zoom))


;;; Info
;;
;; (use-package info
;;   :init
;;   (progn
;;     ;; Additional info directories
;;     (add-to-list 'Info-directory-list "/usr/local/share/info")
;;     (add-to-list 'Info-directory-list "/usr/share/info")
;;     (add-to-list 'Info-directory-list "~/elisp/info")
;;     (add-to-list 'Info-directory-list "~/elisp/info/eshell-doc")
;;     ;; Fancy faces in info.
;;     (defface tv-info-ref-item
;;       '((((background dark)) :background "DimGray" :foreground "Gold")
;;         (((background light)) :background "firebrick" :foreground "LightGray"))
;;       "Face for item stating with -- in info." :group 'Info :group 'faces)

;;     (defvar tv-info-title-face 'tv-info-ref-item)
;;     (defvar tv-info-underline 'underline)
;;     (defvar info-unicode-quote-start (string 8216))
;;     (defvar info-unicode-quote-end (string 8217))
;;     (defvar info-unicode-quoted-regexp (format "[%s]\\([^%s%s]+\\)[%s]"
;;                                                info-unicode-quote-start
;;                                                info-unicode-quote-start
;;                                                info-unicode-quote-end
;;                                                info-unicode-quote-end
;;                                                ))
;;     (defun tv-font-lock-doc-rules ()
;;       (font-lock-add-keywords
;;        nil `(("[^\\s\][`]\\([^`']+\\)[`']?[^\\s\][']?" 1 font-lock-type-face)
;;              (,info-unicode-quoted-regexp 1 font-lock-type-face)
;;              ("^ --.*$" . tv-info-title-face)
;;              ("[_]\\([^_]+\\)[_]" 1 tv-info-underline)
;;              ("[\"]\\([^\"]*\\)[\"]" . font-lock-string-face)
;;              ("\\*Warning:\\*" . font-lock-warning-face)
;;              ("^ *\\([*•]\\) " 1 font-lock-variable-name-face)
;;              ("^[[:upper:],]\\{2,\\}$" . font-lock-comment-face)
;;              ("^[[:upper]][a-z- ]*:" . font-lock-variable-name-face)
;;              )))

;;     (add-hook 'Info-mode-hook 'tv-font-lock-doc-rules)))

;;; emacs-wget site-lisp configuration
;;
;;
 ;; (autoload 'wget "wget" "wget interface for Emacs." t)
 ;; (autoload 'wget-web-page "wget" "wget interface to download whole web page." t)

 ;; (use-package w3m-wget)
 ;; ;; Use wget in eshell.
 ;; (defun eshell/wget (url)
 ;;   (wget url))


;;; Firefox protocol
;;
(autoload 'firefox-protocol-installer-install "firefox-protocol" nil t)

;;; Org
;;
;;(use-package org :ensure t :config (use-package org-config-nagi))
(require 'init-org-nagi)

;; ace-jump-mode
(use-package ace-jump-mode
  :ensure t
  :init
  (define-key global-map (kbd "C-0") 'ace-jump-mode))


;;; Emacs Browser setup
;;
(require 'init-browser-nagi)

;; (use-package w3m
;;   :ensure t
;;   :init (require 'config-w3m)
;;   :bind ("<f7> h" . w3m)
;;   :defer t)

(setq browse-url-browser-function 'helm-browse-url-firefox)
;;(setq browse-url-browser-function 'w3m-browse-url)

;;; FUN GAME
;;
(use-package pacmacs :ensure t)

;; 2048-game
(use-package 2048-game :ensure t)

;;; Emms
;;
(use-package emms
  :ensure t
  :config (use-package emms-vlc-config)
  :commands (emms-stream-init))

;;; Magit
;;
(use-package magit
  :init
  (progn
    (setq magit-status-buffer-name-format "*magit status: %a*")
    (setq magit-restore-window-configuration t)
    (setq git-commit-fill-column 120)
    (setq git-commit-summary-max-length 80)
    (setq auto-revert-verbose nil)
    (setq magit-revision-show-gravatars nil)
    (setq magit-uniquify-buffer-names nil))
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (bind-key "C" 'magit-commit-add-log magit-diff-mode-map)
  (bind-key "C-]" 'magit-toggle-margin magit-log-mode-map)
  :no-require t
  :ensure t)

;;; Eshell-config
;;
(use-package eshell
  :init
  (progn
    ;; Eshell-prompt
    (setq eshell-prompt-function
          #'(lambda nil
              (concat
               (getenv "USER")
               "@"
               (system-name)
               ":"
               (abbreviate-file-name (eshell/pwd))
               (if (= (user-uid) 0) " # " " $ "))))

    ;; Compatibility 24.2/24.3
    (unless (fboundp 'eshell-pcomplete)
      (defalias 'eshell-pcomplete 'pcomplete))
    (unless (fboundp 'eshell-complete-lisp-symbol)
      (defalias 'eshell-complete-lisp-symbol 'lisp-complete-symbol))

    (add-hook 'eshell-mode-hook #'(lambda ()
                                    (setq eshell-pwd-convert-function (lambda (f)
                                                                        (if (file-equal-p (file-truename f) "/")
                                                                            "/" f)))
                                    ;; Helm completion with pcomplete
                                    (setq eshell-cmpl-ignore-case t)
                                    (eshell-cmpl-initialize)
                                    (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
                                    ;; Helm lisp completion
                                    (define-key eshell-mode-map [remap eshell-complete-lisp-symbol] 'helm-lisp-completion-at-point)
                                    ;; Helm completion on eshell history.
                                    (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
                                    ;; Eshell prompt
                                    (set-face-attribute 'eshell-prompt nil :foreground "DeepSkyBlue")
                                    ;; Allow yanking right now instead of returning "Mark set"
                                    (push-mark)))

    ;; Eshell history size
    (setq eshell-history-size 1000) ; Same as env var HISTSIZE.

    ;; Eshell-banner
    (setq eshell-banner-message (format "%s %s\nwith Emacs %s on %s"
                                        (propertize
                                         "Eshell session started on"
                                         'face '((:foreground "Goldenrod")))
                                        (propertize
                                         (format-time-string "%c")
                                         'face '((:foreground "magenta")))
                                        (propertize emacs-version
                                                    'face '((:foreground "magenta")))
                                        (propertize
                                         (with-temp-buffer
                                           (call-process "uname" nil t nil "-r")
                                           (buffer-string))
                                         'face '((:foreground "magenta")))))

    ;; Eshell-et-ansi-color
    (ignore-errors
      (dolist (i (list 'eshell-handle-ansi-color
                       'eshell-handle-control-codes
                       'eshell-watch-for-password-prompt))
        (add-to-list 'eshell-output-filter-functions i)))

    ;; Eshell-save-history-on-exit
    ;; Possible values: t (always save), 'never, 'ask (default)
    (setq eshell-save-history-on-exit t)

    ;; Eshell-directory
    (setq eshell-directory-name "~/.emacs.d/eshell/")

    ;; Eshell-visual
    (setq eshell-term-name "eterm-color")
    (with-eval-after-load "em-term"
      (dolist (i '("tmux" "htop" "alsamixer" "git-log"))
        (add-to-list 'eshell-visual-commands i))))
  :config
  ;; Finally load eshell on startup.
  (add-hook 'emacs-startup-hook #'(lambda ()
                                    (let ((default-directory (getenv "HOME")))
                                      (command-execute 'eshell)
                                      (bury-buffer)))))
(setq eshell-scroll-to-bottom-on-input t)
;; *Note:* hitting return on any other line will copy that line to the
;;     prompt and immediately execute it.




;;; Bookmark+
;;
(use-package bookmark+ :ensure t)






;; Bindings
;;
;; disable alt+space shortcut on unity in settings->keyboard->shortcuts-Windows
;;
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-z")                        nil) ; Disable `suspend-frame'.
(global-set-key (kbd "C-!")                        'eshell-command)
(global-set-key (kbd "C-c R")                      #'(lambda () (interactive) (revert-buffer t t)))
(global-set-key (kbd "C-c W")                      'whitespace-mode)
(global-set-key (kbd "C-M-j")                      #'(lambda () (interactive) (kill-sexp -1)))










;;;; PROGRAMMING

;;; Tag support

;; All programming languages require some sort of tagging. but after
;;    thirty years, we are still using good ol’ ctags...well,
;;    [[http://ctags.sourceforge.net][Exuberant Ctags]].   Install with Homebrew:

;;    #+BEGIN_SRC sh :tangle no
;;      brew install --HEAD ctags
;;    #+END_SRC

;;    On Ubuntu Linux, do:

;;    #+BEGIN_SRC sh :tangle no
;;      sudo apt-get install -y exuberant-ctags
;;    #+END_SRC

;;    Note: for every project, run the following command:

;;    #+BEGIN_SRC sh :tangle no
;;      ctags -e -R .
;;    #+END_SRC

;;    I want to be able to add headers from my =org-mode= files as
;;    a /language option/:

;;    #+BEGIN_SRC sh :tangle ~/.ctags :comments no
;;     --langdef=org
;;     --langmap=org:.org
;;     --regex-org=/^\*+[ \t]+([a-zA-Z0-9_ ]+)/\1/d,definition/
;;    #+END_SRC

;;    We access stuff by loading the =etags= package:

(require 'etags)

;; Now, use the following keys:

;;    - M-. :: To find the tag at point to jump to the function’s
;;             definition when the point is over a function call. It is a
;;             dwim-type function.
;;    - M-, :: jump back to where you were.
;;    - M-? :: find a tag, that is, use the Tags file to look up a
;;             definition. If there are multiple tags in the project with
;;             the same name, use `C-u M-.’ to go to the next match.
;;    - =M-x tags-search= :: regexp-search through the source files
;;         indexed by a tags file (a bit like =grep=)
;;    - =M-x tags-query-replace= :: query-replace through the source files
;;         indexed by a tags file
;;    - =M-x tags-apropos= :: list all tags in a tags file that match a
;;         regexp
;;    - =M-x list-tags= :: list all tags defined in a source file

;;    With the fancy new [[https://marmalade-repo.org/packages/ctags-update][ctags-update]] package, we can update the tags file
;;    whenever we save a file:
(use-package ctags-update
  :ensure t
  :config
  (add-hook 'prog-mode-hook  'turn-on-ctags-auto-update-mode)
  :diminish ctags-auto-update-mode)

;;And if I'm lazy and willing to use the mouse:
(use-package imenu+
  :ensure t
  :init (add-hook 'prog-mode-hook 'imenup-add-defs-to-menubar)
  (add-hook 'org-mode-hook  'imenup-add-defs-to-menubar))

;;If I don't know what I'm after, Helm is better:
(use-package helm
  :bind (("C-c M-i" . helm-imenu)))

(use-package guide-key
       :init (add-to-list 'guide-key/guide-key-sequence "C-x c"))

;; Emacs 25 changed has now deprecated the famous [[info:emacs#Tags][Tags and Friends]],
;;    like =find-tags= for =xref=. Some point, I will have to learn how
;;    to configure it, but until then, I'll just rebind to my old mates:

(global-set-key (kbd "M-.") 'find-tag)
(global-set-key (kbd "C-M-.") 'find-tag-regexp)
(global-set-key (kbd "M-,") 'pop-tag-mark)
(global-set-key (kbd "M-i") 'imenu-anywhere)

;;Note: This prompt needs to go away:
(setq tags-add-tables nil)

;;; Code Block Folding

;; The [[info:emacs#Hideshow][Hide Show Minor]] mode allows us to /fold/ all functions
;;     (hidden), showing only the header lines. We need to turn on the
;;     mode, so wrappers are in order:
(defun ha/hs-show-all ()
  (interactive)
  (hs-minor-mode 1)
  (hs-show-all))

(defun ha/hs-hide-all ()
  (interactive)
  (hs-minor-mode 1)
  (hs-hide-all))

(defun ha/hs-toggle-hiding ()
  (interactive)
  (hs-minor-mode 1)
  (hs-toggle-hiding))

;; Seems that =C-c @= is too obnoxious to use, so I'll put my
;;     favorite on the =C-c h= prefix:
(use-package hs-minor-mode
  :bind
  ("C-c T h" . hs-minor-mode)
  ("C-c h a" . ha/hs-hide-all)
  ("C-c h s" . ha/hs-show-all)
  ("C-c h h" . ha/hs-toggle-hiding))

;;; Web
;;
(require 'init-nagi-web)

;;; js2
;;
(require 'init-javascript-nagi)

;;; using Dash
(if (eq system-type 'darwin)
    (require 'init-mac-dash)
  (require 'init-linux-dash))



(if (window-system)
    (require 'init-client)
  (require 'init-server)

  (server-start))
