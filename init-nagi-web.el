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

(use-package web-mode
  :ensure t
  :bind (("C-c C-v" . browse-url-of-buffer)
         ("C-c w t" . web-mode-element-wrap))
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
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

;;(define-key html-mode-map (kbd "C-c C-w") 'surround-html)


(use-package skewer-mode
  :ensure t
  :commands skewer-mode run-skewer
  :config (skewer-setup))

(use-package impatient-mode
  :ensure t)

(provide 'init-nagi-web)
