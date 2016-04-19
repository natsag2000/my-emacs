;;; Which Function Mode
;;
;; having the name of the function on the line
(setq which-func-unknown "")
(which-function-mode 1)
(setq which-func-format
      `(" "
        (:propertize which-func-current local-map
                     (keymap
                      (mode-line keymap
                                 (mouse-3 . end-of-defun)
                                 (mouse-2 . narrow-to-defun)
                                 (mouse-1 . beginning-of-defun)))
                     face which-func
                     mouse-face mode-line-highlight
                     help-echo "mouse-1: go to beginning\n\
    mouse-2: toggle rest visibility\n\
    mouse-3: go to end")
        " "))

;;; PowerLine
;;
;; PowerLine project can really clean up the mode line
(use-package powerline
  :ensure t)

(custom-set-faces
 '(mode-line-buffer-id ((t (:foreground "#000000" :bold t))))
 '(which-func ((t (:foreground "#77aaff"))))
 '(mode-line ((t (:foreground "#000000" :background "#dddddd" :box nil))))
 '(mode-line-inactive ((t (:foreground "#000000" :background "#bbbbbb" :box nil)))))

(defun powerline-simpler-vc-mode (s)
  (if s
      (replace-regexp-in-string "Git[:-]" "" s)
    s))

;; Some point, we could change the text of the minor modes, but we
;; need to get the text properties and sub them /back in/. To be
;; figured out later... Like:
;;   (let* ((props (text-properties-at 1 s))
;;          (apple (set-text-properties 0 1 props "⌘"))
;;          (fly-c (set-text-properties 0 1 props "✓"))
;;          (news1 (replace-regexp-in-string "񓵸" apple s)))
;;          (news2 (replace-regexp-in-string "FlyC" fly-c news1)))

(defun powerline-simpler-minor-display (s)
  (replace-regexp-in-string
   (concat " " (mapconcat 'identity '("񓵸" "Projectile" "Fill" "BufFace") "\\|")) "" s))

(defun powerline-ha-theme ()
  "A powerline theme that removes many minor-modes that don't serve much purpose on the mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let*
                       ((active
                         (powerline-selected-window-active))
                        (mode-line
                         (if active 'mode-line 'mode-line-inactive))
                        (face1
                         (if active 'powerline-active1 'powerline-inactive1))
                        (face2
                         (if active 'powerline-active2 'powerline-inactive2))
                        (separator-left
                         (intern
                          (format "powerline-%s-%s" powerline-default-separator
                                  (car powerline-default-separator-dir))))
                        (separator-right
                         (intern
                          (format "powerline-%s-%s" powerline-default-separator
                                  (cdr powerline-default-separator-dir))))
                        (lhs
                         (list
                          (powerline-raw "%*" nil 'l)
                          ;; (powerline-buffer-size nil 'l)
                          (powerline-buffer-id nil 'l)
                          (powerline-raw " ")
                          (funcall separator-left mode-line face1)
                          (powerline-narrow face1 'l)
                          (powerline-simpler-vc-mode (powerline-vc face1))))
                        (rhs
                         (list
                          (powerline-raw mode-line-misc-info face1 'r)
                          (powerline-raw "%4l" face1 'r)
                          (powerline-raw ":" face1)
                          (powerline-raw "%3c" face1 'r)
                          (funcall separator-right face1 mode-line)
                          (powerline-raw " ")
                          (powerline-raw "%6p" nil 'r)
                          (powerline-hud face2 face1)))
                        (center
                         (list
                          (powerline-raw " " face1)
                          (funcall separator-left face1 face2)
                          (when
                              (boundp 'erc-modified-channels-object)
                            (powerline-raw erc-modified-channels-object face2 'l))
                          (powerline-major-mode face2 'l)
                          (powerline-process face2)
                          (powerline-raw " :" face2)

                          (powerline-simpler-minor-display
                           (powerline-minor-modes face2 'l))

                          (powerline-raw " " face2)
                          (funcall separator-right face2 face1))))
                     (concat
                      (powerline-render lhs)
                      (powerline-fill-center face1
                                             (/
                                              (powerline-width center)
                                              2.0))
                      (powerline-render center)
                      (powerline-fill face1
                                      (powerline-width rhs))
                      (powerline-render rhs)))))))

(powerline-ha-theme)

(provide 'init-mode-line)
