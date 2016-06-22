;;; Modes
;;

(use-package web-mode
  :ensure t
  :bind (("C-c C-v" . browse-url-of-buffer)
         ("C-c w t" . web-mode-element-wrap))
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
  :config
  (progn
    ;; Set tab to 4 to play nice with plebeian editors
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-code-indent-offset 4)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-enable-current-column-highlight t)))

(use-package mustache-mode
  :ensure t)

;;; Surrounding with Tag
;;

;; Seems that even the SGML mode doesn't care about properly formatted
;; HTML tags. This allows me to select a region and add wrap it in
;; tag...properly.

(defun surround-html (start end tag)
  "Wraps the specified region (or the current 'symbol / word'ef
     with a properly formatted HTML tag."
  (interactive "r\nsTag: " start end tag)
  (save-excursion
    (narrow-to-region start end)
    (goto-char (point-min))
    (insert (format "<%s>" tag))
    (goto-char (point-max))
    (insert (format "</%s>" tag))
    (widen)))

;; And bind it to the HTML hook:
;; (define-key html-mode-map (kbd "C-c C-w") 'surround-html)

;;; Emmet Mode
;;

;; [[https://github.com/smihica/emmet-mode][Emmet-Mode]] is pretty sweet, but need to hook it up to both
;; SGML (which includes HTML) and CSS:

(use-package emmet-mode
  :ensure t
  :commands emmet-mode
  :init
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t)
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  (add-hook 'html-mode-hook 'emmet-mode))


;;; Skewer
;;

;; Live coding for HTML/CSS/JavaScript with a [[https://github.com/skeeto/skewer-mode][Skewer server]] running in Emacs.
;; Useful key-bindings with the =skewer-setup=:

;; - =C-x C-e= :: Evaluate the form before the point and display the result in the
;; - =minibuffer. If given a prefix argument, insert the result into the current
;; - =buffer.
;; - =C-M-x= :: Evaluate the top-level form around the point.
;; - =C-c C-k= :: Load the current buffer.
;; - =C-c C-z= :: Select the REPL buffer.
(use-package skewer-mode
  :ensure t
  :commands skewer-mode run-skewer
  :config (skewer-setup))


;;; Impatient Mode
;;

;; Similar to Skewer, the [[https://github.com/netguy204/imp.el][impatient-mode]] works well with HTML, but not
;; as well with Javascript, so I'm not tangling it right now:

;; Simply turn on the =impatient-mode= for any buffer that should be
;; /served/ and then pop over to http://localhost:8888/imp/
(use-package impatient-mode
  :ensure t)

(provide 'init-nagi-web)
