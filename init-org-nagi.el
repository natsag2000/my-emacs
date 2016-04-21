;;; init-org-nagi.el --- My Org config
;;
;; Author: Nagi
;;
;; Version:
;; URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;; Commentary:
;;  shameless stolen from Howard Abrams and Thierry Volpiatto
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Initial Settings
;;

(use-package org
  :ensure t        ; But it comes with Emacs now!?
  :init
  (setq org-use-speed-commands t
        org-hide-emphasis-markers t
        org-src-fontify-natively t   ;; Pretty code blocks
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil)
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
  (add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode))   ;; Journal entries
  (add-hook 'org-mode-hook 'yas-minor-mode-on)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-M-|" . indent-rigidly))
  :config
  (define-key org-mode-map (kbd "M-C-n") 'org-end-of-item-list)
  (define-key org-mode-map (kbd "M-C-p") 'org-beginning-of-item-list)
  (define-key org-mode-map (kbd "M-C-u") 'outline-up-heading)
  (define-key org-mode-map (kbd "M-C-w") 'org-table-copy-region)
  (define-key org-mode-map (kbd "M-C-y") 'org-table-paste-rectangle)

  (define-key org-mode-map [remap org-return] (lambda () (interactive)
                                                (if (org-in-src-block-p)
                                                    (org-return)
                                                  (org-return-indent)))))

;; *Speed Commands:* If point is at the beginning of a headline or
;; code block in org-mode, single keys do fun things. See
;; =org-speed-command-help= for details (or hit the ? key at a
;;                                          headline).

;; *Note*: For the most part, I like [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Indent-Convenience.html][electric-indent-mode]], however, it
;; doesn't really play well with =org-mode=, so I just bind the Return
;; key to the ~org-return-indent~ function and get the same effect (but
;;                                                                  only if I am /not/ in a source code block...which actually insert
;;                                                                  multiple new lines).  This /return and indent/ feature is fine, since
;; when I save a file, I automatically strip off [[file:emacs.org::*Strip%20Whitespace%20on%20Save][trailing whitespace]].

;; We will use some of the packages from [[http://orgmode.org/worg/org-contrib/][org extras]], especially
;; [[http://orgmode.org/worg/org-contrib/org-drill.html][org-drill]] and [[http://orgmode.org/worg/org-contrib/org-mime.html][org-mime]] for HTML exports:
(use-package org-plus-contrib
  :ensure t)

;;; Local key bindings
;;

;; A couple of short-cut keys to make it easier to edit text.
(defun org-text-bold () "Wraps the region with asterisks."
       (interactive)
       (surround-text "*"))
(defun org-text-italics () "Wraps the region with slashes."
       (interactive)
       (surround-text "/"))
(defun org-text-code () "Wraps the region with equal signs."
       (interactive)
       (surround-text "="))
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "A-b") 'org-text-bold)
            (local-set-key (kbd "s-b") 'org-text-bold)    ;; For Linux
            (local-set-key (kbd "A-i") 'org-text-italics)
            (local-set-key (kbd "s-i") 'org-text-italics)
            (local-set-key (kbd "A-=") 'org-text-code)
            (local-set-key (kbd "s-=") 'org-text-code)))

;;; Color and Display
;;

;; Displaying the headers using various bullets are nice for my presentations.
(use-package org-bullets
  :ensure t
  :init (add-hook 'org-mode-hook 'org-bullets-mode))

;; Here is my approach for quickly making the initial asterisks for
;; listing items and whatnot, appear as Unicode bullets (without
;;                                                       actually affecting the text file or the behavior).
(use-package org-mode
  :init
  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

;;; Journaling
;;

;; Didn't realize that [[http://www.emacswiki.org/emacs/OrgJournal][org-journal]] essentially does what I have been
;; doing by hand. With a little customization, I don't have to change
;; anything else:
(use-package org-journal
  :ensure t
  :init
  (setq org-journal-dir "~/org/journal/")
  (setq org-journal-date-format "#+TITLE: Өдрийн тэмдэглэл - %Y-%b-%d (%A)")
  (setq org-journal-time-format ""))

;; The time format is the heading for each section. I set it to a
;; blank since I really don't care about the time I add a section.

;; Nice to /automatically/ insert a specific header if the journal entry
;; file is empty using [[https://www.gnu.org/software/emacs/manual/html_node/autotype/Autoinserting.html][auto-insert]].

;; A function to easily load today (and yesterday's) journal entry.

(defun get-journal-file-today ()
  "Return filename for today's journal entry."
  (let ((daily-name (format-time-string "%Y%m%d")))
    (expand-file-name (concat org-journal-dir daily-name))))

(defun journal-file-today ()
  "Create and load a journal file based on today's date."
  (interactive)
  (find-file (get-journal-file-today)))

(global-set-key (kbd "C-c f j") 'journal-file-today)

;; Since I sometimes (not often) forget to create a journal entry,
;; and need to re-write history.
(defun get-journal-file-yesterday ()
  "Return filename for yesterday's journal entry."
  (let ((daily-name (format-time-string "%Y%m%d" (time-subtract (current-time) (days-to-time 1)))))
    (expand-file-name (concat org-journal-dir daily-name))))

(defun journal-file-yesterday ()
  "Creates and load a file based on yesterday's date."
  (interactive)
  (find-file (get-journal-file-yesterday)))

(global-set-key (kbd "C-c f y") 'journal-file-yesterday)

;; Seems like I need to have the inserted template match the file's
;; name, not necessarily today's date:
(defun journal-file-insert ()
  "Insert's the journal heading based on the file's name."
  (interactive)
  (when (string-match "\\(20[0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)" (buffer-name))
    (let ((year  (string-to-number (match-string 1 (buffer-name))))
          (month (string-to-number (match-string 2 (buffer-name))))
          (day   (string-to-number (match-string 3 (buffer-name))))
          (datim nil))
      (setq datim (encode-time 0 0 0 day month year))
      (insert (format-time-string org-journal-date-format datim))
      (insert "\n\n"))))  ; Start with a blank separating line

(add-to-list 'auto-insert-alist '(".*/[0-9]*$" . journal-file-insert))

;; I really would really like to read what I did last year "at this
;;   time", and by that, I mean, 365 days ago, plus or minus a few to get
;; to the same day of the week.

(defun journal-last-year-file ()
  "Returns the string corresponding to the journal entry that
    happened 'last year' at this same time (meaning on the same day
    of the week)."
  (let* ((last-year-seconds (- (float-time) (* 365 24 60 60)))
         (last-year (seconds-to-time last-year-seconds))
         (last-year-dow (nth 6 (decode-time last-year)))
         (this-year-dow (nth 6 (decode-time)))
         (difference (if (> this-year-dow last-year-dow)
                         (- this-year-dow last-year-dow)
                       (- last-year-dow this-year-dow)))
         (target-date-seconds (+ last-year-seconds (* difference 24 60 60)))
         (target-date (seconds-to-time target-date-seconds)))
    (format-time-string "%Y%m%d" target-date)))

(defun journal-last-year ()
  "Loads last year's journal entry, which is not necessary the
    same day of the month, but will be the same day of the week."
  (interactive)
  (let ((journal-file (concat org-journal-dir (journal-last-year-file))))
    (find-file journal-file)))

(global-set-key (kbd "C-c f L") 'journal-last-year)

;;; TODO: taking meeting notes
;;

;;; Sepcify the Org Directories
;;
;; I keep all my =org-mode= files in a few directories, and I would
;; like them automatically searched when I generate agendas.

(setq org-directory "~/org")
(setq org-agenda-files '("~/org/personal"
                         "~/org/technical"
                         "~/org/project"))

;;; Auto Note Capturing
;;
;; TODO: learn and then configure it nicely for yourself
;; After you have selected the template, you type in your note and hit
;;   =C-c C-c= to store it in the file listed above.
;;
;; Just remember, at some point to hit =C-c C-w= to /refile/ that note
;; in the appropriate place.

(defvar org-default-notes-file "~/org/personal/notes.org")
(defvar org-default-tasks-file "~/org/personal/tasks.org")

(setq org-tag-alist '(("@entrainement")
                      ("climbing")
                      ("equipement")
                      ("@github")
                      ("helm")
                      ("async")
                      ("crypt")
                      ("@home")
                      ("@travel")))

(setq org-capture-templates
      '(("t" "Todo" entry         (file+headline  org-default-tasks-file "Tasks")        "** TODO %?\n  %i\n  %a"  :prepend t)
        ("n" "Notes" entry        (file+headline  org-default-notes-file "General")      "* %T %?\n\n  %i\n"       :prepend t)
        ("E" "Entrainement" entry (file+headline  org-default-notes-file "Entrainement") "* %T %?\n\n  %i\n"       :prepend t)
        ("H" "Helm" entry         (file+headline  org-default-notes-file "Helm")         "* %^{Title}\n  %i\n  %a" :prepend t)
        ("l" "Lisp" entry         (file+headline  org-default-notes-file "Elisp")        "* %^{Title}\n  %i\n  %a" :prepend t)
        ("p" "Python" entry       (file+headline  org-default-notes-file "Python")       "* %^{Title}\n  %i\n  %a" :prepend t)
        ("b" "Bash" entry         (file+headline  org-default-notes-file "Bash")         "* %^{Title}\n  %i\n  %a" :prepend t)
        ("L" "Linux" entry        (file+headline  org-default-notes-file "Linux")        "* %^{Title}\n  %i\n  %a" :prepend t)))

;;; Export Settings
;;

;; Seems some change now requires a direct load of HTML:

;; To make the =org-mode= export defaults closer to my liking
;; (without having to put specific #+PROPERTY commands), I get rid of
;; the postamble, and then configure the default fonts.
(use-package ox-html
  :init
  (setq org-html-postamble nil)
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-toc nil)
  (setq org-html-head-extra "
         <link href='http://fonts.googleapis.com/css?family=Source+Sans+Pro:400,700,400italic,700italic&subset=latin,latin-ext' rel='stylesheet' type='text/css'>
         <link href='http://fonts.googleapis.com/css?family=Source+Code+Pro:400,700' rel='stylesheet' type='text/css'>
         <style type='text/css'>
            body {
               font-family: 'Source Sans Pro', sans-serif;
            }
            pre, code {
               font-family: 'Source Code Pro', monospace;
            }
         </style>"))

;;; Presentations
;;
;; alternated between the browser-based presentation tool, reveal.js
;; and staying in Emacs with org-tree-slide
;;
;;; REVEAL part
;; Generate presentations from my org-mode files using
;; [[https://github.com/yjwen/org-reveal][org-reveal]]. Just download and make the results available to the
;; HTML output:
(use-package ox-reveal
  :ensure t
  :init
  (setq org-reveal-root (concat "file://" (getenv "HOME") "/Public/js/reveal.js"))
  (setq org-reveal-postamble "Natsagdorj Shagdar"))

;; Tree Slide part
;;
;; A quick way to display an org-mode file is using [[https://github.com/takaxp/org-tree-slide][org-tree-slide]].

;;    * org-tree-slide-move-next-tree (C->)
;;    * org-tree-slide-move-previous-tree (C-<)
;;    * org-tree-slide-content (C-x s c)
(use-package org-tree-slide
  :ensure t
  :init
  (setq org-tree-slide-skip-outline-level 4)
  (org-tree-slide-simple-profile))

;; TODO: import literate programming part from Abrams config here


;;; finished here are other configs from thiel TODO: clean it up

;; auto-fill-mode
;; (set to 78 in files)
(add-hook 'org-mode-hook 'auto-fill-mode) ;; set in Fill Mode section in emacs.el

;; Use-enter-to-follow-links
(setq org-return-follows-link t)

;; Todo-rules
;; (find-node "(org)Fast access to TODO states")
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "INPROGRESS(i)" "DONE(d)" "CANCELED(c)" "DEFERRED(s)")))

(setq org-todo-keyword-faces
      '(("TODO"      .  ((:foreground "red")))
        ("INPROGRESS" . ((:foreground "yellow")))
        ("BUGREPORT" . ((:foreground "VioletRed4" :weight bold)))
        ("FIXED" . ((:foreground "SpringGreen4" :weight bold)))
        ("DEFERRED"  . shadow)
        ("CANCELED"  . ((:foreground "blue" :weight bold)))))


(setq org-log-done 'time)
(setq org-use-fast-todo-selection t)
(setq org-reverse-note-order t)


;; org-annotation-helper
(use-package org-annotation-helper)

;; Diary-integration-in-org
(setq org-agenda-include-diary t) ; show also content of regular diary file.

;; Insinuate-appt
(use-package appt
    :config
  (progn
    ;; When use 'r' (rebuild agenda) reload appt
    (add-hook 'org-agenda-mode-hook #'(lambda ()
                                        (setq appt-time-msg-list nil)
                                        (define-key org-agenda-mode-map (kbd "C-c M") 'org-agenda-month-view)))
    (setq appt-display-format 'window) ; Values: 'echo, 'window or nil.
    (appt-activate 1))
  :defer t)

;; Subtasks
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; leave-a-blank-line-when-insert-new-item
(setq org-blank-before-new-entry '((heading . t)
                                   (plain-list-item . nil)))

(defun tv/insert-org-src-keyword (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (insert "#+begin_src\n")
    (goto-char end)
    (forward-line 1)
    (insert "\n#+end_src")))

(add-hook 'org-mode-hook
	  (lambda ()
	    (define-key org-mode-map (kbd "<f11> o") 'helm-org-in-buffer-headings)
	    (define-key org-mode-map (kbd "<f11> k") 'tv/insert-org-src-keyword)))

;; Colorize-Diary's-entries-in-agenda
(defvar tv-diary-regexp "^ *[Dd]iary")
(defvar tv-diary-done-regexp "^ *[Dd]iary.* *==> *done$")
(defun tv-org-propertize-diary-entries ()
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (not (eobp))
        (cond ((re-search-forward tv-diary-done-regexp (point-at-eol) t)
               (add-text-properties
                (point-at-bol) (point-at-eol) '(face 'tv-org-diary-done)))
              ((re-search-forward tv-diary-regexp (point-at-eol) t)
               (add-text-properties
                (point-at-bol) (point-at-eol) '(face tv-org-diary))))
        (forward-line 1)))))
(add-hook 'org-finalize-agenda-hook 'tv-org-propertize-diary-entries)

(defface tv-org-diary '((t (:foreground "Yellow3" :underline t)))
  "*Face for diary entry in org agenda."
  :group 'org)

(defface tv-org-diary-done '((t (:foreground "darkgrey")))
  "*Face for finished diary entry in org agenda."
  :group 'org)

(defun tv-org-propertize-note-entry ()
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (re-search-forward "^ +note" nil t)
        (add-text-properties (match-beginning 0) (point-at-eol)
                             '(face '((:foreground "magenta"))))))))
(add-hook 'org-finalize-agenda-hook 'tv-org-propertize-note-entry)


(add-hook 'org-agenda-mode-hook
	  (lambda () (set-face-attribute 'org-agenda-date-weekend nil :foreground "red")))

;; org-crypt
(use-package org-crypt
    :config
  (progn
    (org-crypt-use-before-save-magic)
    (setq org-crypt-key "59F29997")
    (setq org-crypt-disable-auto-save t) ;'encrypt)
    (define-key org-mode-map (kbd "C-c e") 'org-encrypt-entry)
    (define-key org-mode-map (kbd "C-c d") 'org-decrypt-entry)))

;; fontify source code
(setq org-src-fontify-natively t)

;; Always show full path of files
(setq org-link-file-path-type 'absolute)

;; Org babel
(use-package ob-sh)
(use-package ob-emacs-lisp)


(provide 'init-org-nagi)

;;; org-config-nagi.el ends here
