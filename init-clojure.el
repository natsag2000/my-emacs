;;; clojure mode-line
;;

(use-package clojure-mode
  :ensure t
  :init
  (defconst clojure--prettify-symbols-alist
    '(("fn"   . ?λ)
      ("__"   . ?⁈)))

  :config
  (add-hook 'clojure-mode-hook 'global-prettify-symbols-mode))

;; Need to figure out how to get the color identifiers mode to work
;; without an error:

(use-package color-identifiers-mode
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'color-identifiers-mode))

;;; Compojure
;;

;; According to the [[https://github.com/weavejester/compojure/wiki][Compojure Wiki]], the following code makes their
;; macros look prettier:

(use-package clojure-mode
  :config
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)))

;;; Paredit

;; All Lisps, including Clojure, should use [[http://www.emacswiki.org/emacs/ParEdit][paredit]].

;; Since it’s currently possible to use something like =join-lines=
;; to pull code up from one line and stick it into the end-of-line
;; comment of another line, invalidating the code. [[http://www.emacswiki.org/emacs/ParEdit#toc7][The following]]
;; replacement for [[help:delete-indentation][delete-indentation]] prevents this.

(defun paredit-delete-indentation (&optional arg)
  "Handle joining lines that end in a comment."
  (interactive "*P")
  (let (comt)
    (save-excursion
      (move-beginning-of-line (if arg 1 0))
      (when (skip-syntax-forward "^<" (point-at-eol))
        (setq comt (delete-and-extract-region (point) (point-at-eol)))))
    (delete-indentation arg)
    (when comt
      (save-excursion
        (move-end-of-line 1)
        (insert " ")
        (insert comt)))))

;; While =M-SPC= (especially =M-0 M-SPC=) is good for cleaning up extra
;; white space on a single line, let's use this function to get rid of
;; it all.

(defun paredit-remove-newlines ()
  "Removes extras whitespace and newlines from the current point
    to the next parenthesis."
  (interactive)
  (let ((up-to (point))
        (from (re-search-forward "[])}]")))
    (backward-char)
    (while (> (point) up-to)
      (paredit-delete-indentation))))

;; Bind these previous functions and add it to the =clojure-mode=:

(use-package paredit
  :bind ("M-^" . paredit-delete-indentation)
  :bind ("C-^" . paredit-remove-newlines)
  :init
  (add-hook 'clojure-mode-hook 'paredit-mode))

;; Useful key sequences for positioning cursor on particular s-expressions:

;; - C-M- a d :: Move to beginning of function and inside the
;; declaration. Good start to just about any other positioning.
;; - C-M- d f d :: At beginning of function, moves to first s-expression.

;;; REPL
;;

;; When demonstrating Clojure, I find it is a better approach is to send
;; the S-Expression to the REPL and evaluate it there instead of
;; showing the result in the mini-buffer:

(defun cider-send-and-evaluate-sexp ()
  "Sends the s-expression located before the point or the active
      region to the REPL and evaluates it. Then the Clojure buffer is
      activated as if nothing happened."
  (interactive)
  (if (not (region-active-p))
      (cider-insert-last-sexp-in-repl)
    (cider-insert-in-repl
     (buffer-substring (region-beginning) (region-end)) nil))
  (cider-switch-to-repl-buffer)
  (cider-repl-closing-return)
  (cider-switch-to-last-clojure-buffer)
  (message ""))

;;; Cider
;;

;; The [[https://github.com/clojure-emacs/cider][Cider project]] is da bomb. Usage:

;; - =cider-jack-in= - For starting an nREPL server and setting
;; everything up. Keyboard: =C-c M-j=
;; - =cider= to connect to an existing nREPL server.

(use-package cider
  :ensure t
  :commands (cider cider-connect cider-jack-in)

  :init
  (setq cider-auto-select-error-buffer t
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-use-clojure-font-lock t
        cider-repl-wrap-history t
        cider-repl-history-size 1000
        cider-show-error-buffer t
        nrepl-hide-special-buffers t
        ;; Stop error buffer from popping up while working in buffers other than the REPL:
        nrepl-popup-stacktraces nil)

  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-mode-hook 'company-mode)

  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'superword-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'cider-test-report-mode 'jcf-soft-wrap)

  (bind-key "C-x C-e" 'cider-eval-last-sexp clojure-mode-map)
  (bind-key "C-c C-v" 'cider-send-and-evaluate-sexp)

  :config
  (use-package slamhound))

;; While I typically use [[https://github.com/clojure-emacs/clj-refactor.el][clj-refactor]]'s [[https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-missing-libspec][add-missing-libspec]] function,
;; I am thinking of looking into [[https://github.com/technomancy/slamhound][Slamhound]] for reconstructing the =ns= namespace.

;; This also specifies using [[http://emacswiki.org/emacs/ElDoc][ElDoc]] working with Clojure.

;; To get Clojure's Cider working with org-mode, do:

(use-package ob-clojure
  :init
  (setq org-babel-clojure-backend 'cider))


;;; Linting

;; Using [[https://github.com/jonase/eastwood#emacs--cider][Eastwood]] with the [[https://github.com/clojure-emacs/squiggly-clojure][Squiggly Clojure]] project to add lint
;; warnings to [[file:emacs.org::*Flycheck][Flycheck]]:

(use-package flycheck-clojure
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (use-package flycheck
    :config
    (flycheck-clojure-setup)))

;; Seems we should also install [[https://github.com/flycheck/flycheck-pos-tip][flycheck-pos-tip]] as well.

(use-package flycheck-pos-tip
  :ensure t
  :config
  (use-package flycheck
    :config
    (setq flycheck-display-errors-function 'flycheck-pos-tip-error-messages)))

;;; Refactoring
;;

;; Using the [[https://github.com/clojure-emacs/clj-refactor.el][clj-refactor]] project:

(use-package clj-refactor
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  :config
  ;; Configure the Clojure Refactoring prefix:
  (cljr-add-keybindings-with-prefix "C-c .")
  :diminish clj-refactor-mode)

;; The advanced refactorings require the [[https://github.com/clojure-emacs/refactor-nrepl][refactor-nrepl middleware]],
;; which should explain why we added the =refactor-nrepl= to the
;; =:plugins= section in the =~/.lein/profiles.clj= file.

;; Of course, the /real problem/ is trying to remember all the
;; [[https://github.com/clojure-emacs/clj-refactor.el/wiki][refactoring options]]. Remember: =C-c . h h=

;;; Clojure Docs
;;
;; So many Clojure documentation resources, what is a hacker to do?
;; Use [[https://github.com/abo-abo/hydra][Hydra]], of course!

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-clojure-docs (clojure-mode-map "C-c d" :color blue)
    "Clojure Documentation"
    ("f" cider-code "functional")
    ("g" cider-grimoire "grimoire")
    ("w" cider-grimoire-web "web examples")
    ("c" clojure-cheatsheet "cheatsheet")
    ("d" dash-at-point "dash")))

;;; 4Clojure
;;
;; Finally, if you are just learning Clojure, check out [[http://www.4clojure.com/][4Clojure]] and then
;; install [[https://github.com/joshuarh/4clojure.el][4clojure-mode]].

(use-package 4clojure
  :init
  (bind-key "<f9> a" '4clojure-check-answers clojure-mode-map)
  (bind-key "<f9> n" '4clojure-next-question clojure-mode-map)
  (bind-key "<f9> p" '4clojure-previous-question clojure-mode-map)

  :config
  (defadvice 4clojure-open-question (around 4clojure-open-question-around)
    "Start a cider/nREPL connection if one hasn't already been started when
         opening 4clojure questions."
    ad-do-it
    (unless cider-current-clojure-buffer
      (cider-jack-in))))


(provide 'init-clojure)
