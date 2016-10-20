(use-package nvm
  :ensure t)

(nvm-use (caar (last (nvm--installed-versions))))

(defvar flycheck-javascript-eslint-executable)

(defun mjs/setup-local-eslint ()
  "If ESLint found in node_modules directory - use that for flycheck.
  Intended for use in PROJECTILE-AFTER-SWITCH-PROJECT-HOOK."
  (interactive)
  (let ((local-eslint (expand-file-name "./node_modules/.bin/eslint")))
    (setq flycheck-javascript-eslint-executable
          (and (file-exists-p local-eslint) local-eslint))))

(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))

(with-eval-after-load 'web-mode
  ;; set reasoable indentation for web-mode
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)

  (with-eval-after-load 'flycheck
    (push 'web-mode (flycheck-checker-get 'javascript-eslint 'modes))))

(with-eval-after-load 'js
  (setq js-indent-level 2))


;; add own snippets
(add-to-list 'load-path "~/org/mysnippets/react-snippets")
(require 'nagi-react-snippets)

(provide 'init-react)
