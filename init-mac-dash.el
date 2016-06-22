(use-package dash-at-point
       :ensure t
       :init
       (global-set-key (kbd "C-c d") 'dash-at-point-with-docset)
       :config
       (add-to-list 'dash-at-point-mode-alist '(clojure-mode . "clojure"))
       (add-to-list 'dash-at-point-mode-alist '(ruby-mode . "ruby"))
       (add-to-list 'dash-at-point-mode-alist '(python-mode . "python2"))
       (add-to-list 'dash-at-point-mode-alist '(sh-mode . "bash"))
       (add-to-list 'dash-at-point-mode-alist '(emacs-lisp-mode . "elisp")))

(provide 'init-mac-dash)
