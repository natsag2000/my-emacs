;;; Eshell-config
;;
(use-package eshell
  :init
  (progn
    ;; Eshell-prompt
    (setq eshell-prompt-function
          #'(lambda nil
              (concat
               (getenv "USER")
               "@"
               (system-name)
               ":"
               (abbreviate-file-name (eshell/pwd))
               (if (= (user-uid) 0) " # " " $ "))))

    ;; Compatibility 24.2/24.3
    (unless (fboundp 'eshell-pcomplete)
      (defalias 'eshell-pcomplete 'pcomplete))
    (unless (fboundp 'eshell-complete-lisp-symbol)
      (defalias 'eshell-complete-lisp-symbol 'lisp-complete-symbol))
    ;; some aliases
    (defalias 'e 'find-file)
    (defalias 'ff 'find-file)
    (defalias 'emacs 'find-file)
    ;; Replacing the window with the new buffer may not be what I want.
    (defalias 'ee 'find-file-other-window)
    ;;  Pull up dired, but without parameters, just use the current directory.
    (defun eshell/d (&rest args)
      (dired (pop args) "."))
    (add-hook 'eshell-mode-hook #'(lambda ()
                                    (setq eshell-pwd-convert-function (lambda (f)
                                                                        (if (file-equal-p (file-truename f) "/")
                                                                            "/" f)))
                                    ;; Helm completion with pcomplete
                                    (setq eshell-cmpl-ignore-case t)
                                    (eshell-cmpl-initialize)
                                    (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
                                    ;; Helm lisp completion
                                    (define-key eshell-mode-map [remap eshell-complete-lisp-symbol] 'helm-lisp-completion-at-point)
                                    ;; Helm completion on eshell history.
                                    (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
                                    ;; Eshell prompt
                                    (set-face-attribute 'eshell-prompt nil :foreground "DeepSkyBlue")
                                    (eshell/alias "ll" "ls -Alh $*")
                                    ;; Allow yanking right now instead of returning "Mark set"
                                    (push-mark)))
    ;; I can never seem to remember that =find= and =chmod= behave
    ;; differently from Emacs than their Unix counterparts, so at this
    ;; time, I will prefer the native implementations.
    (setq eshell-prefer-lisp-functions nil)

    ;; Eshell history size
    (setq eshell-history-size 1000) ; Same as env var HISTSIZE.

    ;; Eshell-banner
    (setq eshell-banner-message (format "%s %s\nwith Emacs %s on %s"
                                        (propertize
                                         "Eshell session started on"
                                         'face '((:foreground "Goldenrod")))
                                        (propertize
                                         (format-time-string "%c")
                                         'face '((:foreground "magenta")))
                                        (propertize emacs-version
                                                    'face '((:foreground "magenta")))
                                        (propertize
                                         (with-temp-buffer
                                           (call-process "uname" nil t nil "-r")
                                           (buffer-string))
                                         'face '((:foreground "magenta")))))

    ;; Eshell-et-ansi-color
    (ignore-errors
      (dolist (i (list 'eshell-handle-ansi-color
                       'eshell-handle-control-codes
                       'eshell-watch-for-password-prompt))
        (add-to-list 'eshell-output-filter-functions i)))

    ;; Eshell-save-history-on-exit
    ;; Possible values: t (always save), 'never, 'ask (default)
    (setq eshell-save-history-on-exit t)

    ;; Eshell-directory
    (setq eshell-directory-name "~/.emacs.d/eshell/")

    ;; Eshell-visual
    (setq eshell-term-name "eterm-color")
    (with-eval-after-load "em-term"
      (dolist (i '("tmux" "htop" "alsamixer" "git-log" "npm"))
        (add-to-list 'eshell-visual-commands i))))
  :config
  ;; Finally load eshell on startup.
  (add-hook 'emacs-startup-hook #'(lambda ()
                                    (let ((default-directory (getenv "HOME")))
                                      (command-execute 'eshell)
                                      (bury-buffer)))))
(setq eshell-scroll-to-bottom-on-input t)
;; *Note:* hitting return on any other line will copy that line to the
;;     prompt and immediately execute it.

(provide 'init-eshell-nagi)
