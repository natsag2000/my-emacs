;; JavaScript should have three parts:
;; - Syntax highlight (already included)
;; - Syntax verification (with flycheck)
;; - Interactive REPL ... using Skewer

;;; JS2 Mode
;;

;;I like the extras found in [[http://www.emacswiki.org/emacs-test/SteveYegge][Steve Yegge]]'s [[https://github.com/mooz/js2-mode][js2-mode]].

(use-package js2-mode
  :ensure t
  :init
  (setq js-basic-indent 2)
  (setq-default js2-basic-indent 2
                js2-basic-offset 2
                js2-auto-indent-p t
                js2-cleanup-whitespace t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t
                js2-global-externs (list "window" "module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "jQuery" "$"))

  (add-hook 'js2-mode-hook
            (lambda ()
              (push '("function" . ?ƒ) prettify-symbols-alist)))

  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

;;Color /defined/ variables with [[https://github.com/ankurdave/color-identifiers-mode][color-identifiers-mode]]:
(use-package color-identifiers-mode
  :ensure t
  :init
  (add-hook 'js2-mode-hook 'color-identifiers-mode))

(use-package json-mode :ensure t)
(use-package exec-path-from-shell :ensure t)

;;; Flycheck and JSHint
;;

;; While editing JavaScript is baked into Emacs, it is quite important
;; to have [[http://flycheck.readthedocs.org/][flycheck]] validate the source based on [[http://www.jshint.com/][jshint]], and [[https://github.com/eslint/eslint][eslint]].
;; Let’s prefer =eslint=:

(add-hook 'js2-mode-hook
          (lambda () (flycheck-select-checker "javascript-eslint")))

;;; Refactoring JavaScript
;;

;; The [[https://github.com/magnars/js2-refactor.el][js2-refactor]] mode should start with =C-c .= and then a two-letter
;; mnemonic shortcut.

;; * =ef= is =extract-function=: Extracts the marked expressions out into a new named function.
;; * =em= is =extract-method=: Extracts the marked expressions out into a new named method in an object literal.
;; * =ip= is =introduce-parameter=: Changes the marked expression to a parameter in a local function.
;; * =lp= is =localize-parameter=: Changes a parameter to a local var in a local function.
;; * =eo= is =expand-object=: Converts a one line object literal to multiline.
;; * =co= is =contract-object=: Converts a multiline object literal to one line.
;; * =eu= is =expand-function=: Converts a one line function to multiline (expecting semicolons as statement delimiters).
;; * =cu= is =contract-function=: Converts a multiline function to one line (expecting semicolons as statement delimiters).
;; * =ea= is =expand-array=: Converts a one line array to multiline.
;; * =ca= is =contract-array=: Converts a multiline array to one line.
;; * =wi= is =wrap-buffer-in-iife=: Wraps the entire buffer in an immediately invoked function expression
;; * =ig= is =inject-global-in-iife=: Creates a shortcut for a marked global by injecting it in the wrapping immediately invoked function expression
;; * =ag= is =add-to-globals-annotation=: Creates a =/*global */= annotation if it is missing, and adds the var at point to it.
;; * =ev= is =extract-var=: Takes a marked expression and replaces it with a var.
;; * =iv= is =inline-var=: Replaces all instances of a variable with its initial value.
;; * =rv= is =rename-var=: Renames the variable on point and all occurrences in its lexical scope.
;; * =vt= is =var-to-this=: Changes local =var a= to be =this.a= instead.
;; * =ao= is =arguments-to-object=: Replaces arguments to a function call with an object literal of named arguments. Requires yasnippets.
;; * =3i= is =ternary-to-if=: Converts ternary operator to if-statement.
;; * =sv= is =split-var-declaration=: Splits a =var= with multiple vars declared, into several =var= statements.
;; * =uw= is =unwrap=: Replaces the parent statement with the selected region.

(use-package js2-refactor
  :ensure t
  :init   (add-hook 'js2-mode-hook 'js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c ."))

;;; Skewer
;;

;; I also configure Skewer for my [[file:emacs-web.org][HTML and CSS]] files, we need to do the
;; same for JavaScript:

;; Kick things off with =run-skewer=, and then:

;; * C-x C-e :: `skewer-eval-last-expression'
;; * C-M-x   :: `skewer-eval-defun'
;; * C-c C-k :: `skewer-load-buffer'

(use-package skewer-mode
  :ensure t
  :init (add-hook 'js2-mode-hook 'skewer-mode))

;;; Coffee
;;

;;Using the [[https://github.com/defunkt/coffee-mode][coffee-mode]] for CoffeeScript file.
;; Need to remember the following keybindings:

;; - =Return= :: Insert newline and indent line
;; - =C-c C-<, backtab= :: Indent line or region to left
;; - =C-c C->= :: Indent line or region to right
;; - =A-r, C-c C-k= :: Compile buffer to JavaScript
;; - =A-R= :: Compile content of region to JavaScript
;; - =A-M-r, C-c C-z= :: Run CoffeeScript REPL
;; - =C-c C-l= :: Send this line to REPL buffer
;; - =C-c C-r= :: Send content of region to REPL buffer
;; - =C-c C-b= :: Send content of buffer to REPL buffer
;; - =C-c C-o C-s= :: Enable coffee-cos-mode
;; (use-package coffee-mode
;;   :ensure t
;;   :init
;;   (setq-default coffee-tab-width 2))

(provide 'init-javascript-nagi)
