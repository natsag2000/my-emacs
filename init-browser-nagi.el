;;; Browser setting
;; NOTE: w3m package must be installed!!!
;; try also EWW
;; TODO: use some config here from  old (w3m) configuration

;; While browsing, remember the following:

;;     - TAB to jump from link to link.
;;     - RETURN to follow a link
;;     - SPACE to move down the page
;;     - b to move up the page
;;     - B to move back in the history
;;     - M to open the URL in Firefox
;;     - I to open the image if it didn't show up correctly
;;     - c to copy the URL of the current page in the kill ring.
;;     - u to copy the URL of the link in the kill ring.
;;     - a to bookmark this page
;;     - v to look at the bookmarks
;;     - s to look through the page history for this session.

(use-package w3m
  :ensure t
  :commands w3m-goto-url w3m-search
  :init
  (setq browse-url-browser-function 'w3m-browse-url)
  (setq w3m-use-cookies t)

  ;; clean up the w3m buffers:
  (add-hook 'w3m-display-functions 'w3m-hide-stuff)
  (add-hook 'w3m-mode 'ace-link-mode)

  (global-set-key (kbd "C-c w w") 'w3m-goto-url)
  (global-set-key (kbd "C-c w l") 'browse-url-at-point)
  (global-set-key (kbd "C-c w g") 'w3m-search)

  :config
  (define-key w3m-mode-map (kbd "&") 'w3m-view-url-with-external-browser))


;;;; External Web Browsing
;;;
;; Need to be able to switch and have a link in an =org-mode= file show
;; up in the default, graphical browser:

(defun ha-switch-default-browser ()
  "Switches the default browser between the internal and external web browser."
  (interactive)
  ;;         | Variable                  | Function
  (if (equal browse-url-browser-function 'browse-url-default-browser)
      (if (fboundp 'w3m)
          (setq browse-url-browser-function 'w3m-browse-url)
        (setq browse-url-browser-function 'eww-browse-url))
    (setq browse-url-browser-function 'browse-url-default-browser))

  ;; Now we need to display the current setting. The variables are
  ;; pretty typical and have the goodies, but I just need to get rid
  ;; of the word "url" or "browser", and the results are pretty close:
  (message "Browser set to: %s"
           (car
            (filter (lambda (x)
                      (if (or (equal "url" x)
                              (equal "browse" x)
                              (equal "browser" x))
                          nil
                        t))
                    (split-string (format "%s" browse-url-browser-function) "-")))))

(global-set-key (kbd "C-c w d") 'ha-switch-default-browser)

;;;;;TODO: more filtering stuffs from abraham here!!


(use-package ace-link
  :ensure t
  :config
  (ace-link-setup-default))


(provide 'init-browser-nagi)
