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

;;; Code:

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

;;; Org extras usefull for exporting HTML
;;
(use-package org-plus-contrib
  :ensure t)

;;; Local key bindings
;;
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
(use-package org-bullets
  :ensure t
  :init (add-hook 'org-mode-hook 'org-bullets-mode))
;; asterisk is now displayed as a bullet
(use-package org-mode
  :init
  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

;;; TODO: maybe insert org-journaling here
;;

;;; Sepcify the Org Directories
;;

(setq org-directory "~/org")
(setq org-agenda-files '("~/org/personal"
                         "~/org/technical"
                         "~/org/project"))

;;; Auto Note Capturing
;;
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

;; auto-fill-mode
;; (set to 78 in files)
;;(add-hook 'org-mode-hook 'auto-fill-mode) ;; set in Fill Mode section in emacs.el

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
