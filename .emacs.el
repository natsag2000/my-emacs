(setq user-full-name "Natsagdorj Shagdar (nagi)")
(setq user-mail-address "")

;;(setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/usr/bin:/bin" (getenv "PATH")))
(require 'cl-lib)



(load "package")
(package-initialize)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ))

(setq package-pinned-packages '((async . "melpa")
                                (magit . "melpa-stable")
                                (magit-popup . "melpa-stable")))

;;; use-package
;;
(eval-when-compile (require 'use-package))
(setq use-package-verbose t)

;;; Global settings
;;
;; confirm-quit-emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; case-sensitivity
;; nil means case-sensitive; non-nil means case-insensitive
(setq case-fold-search t)

;; Add-newline-at-end-of-files
(setq require-final-newline t)

;; y-or-n-p
(fset 'yes-or-no-p 'y-or-n-p)

;; Affiche-l'heure-au-format-24h
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)
(setq display-time-use-mail-icon t)

;; Limite-max-lisp
(setq max-lisp-eval-depth 40000)
(setq max-specpdl-size 100000)

;; Increase GC
(setq gc-cons-threshold 20000000)

;; Annoyances section
;;
(global-set-key (kbd "<f11>") nil)
(add-hook 'emacs-startup-hook #'(lambda ()
                                  (when (get-buffer "*Compile-Log*")
                                    (kill-buffer "*Compile-Log*")
                                    (delete-other-windows))))

;; Disable uniquify enabled by default in 24.4.
(setq uniquify-buffer-name-style nil)

;; electric-indent-mode
(electric-indent-mode -1)

(setq register-preview-delay nil)

;; No-startup-screen
(setq inhibit-startup-message t)

;; consequent-log-file
(setq message-log-max 1000)

;; kill-ring
(setq kill-ring-max 60)

;; mark ring
(setq mark-ring-max 60)


;; Kill emacs
(defun nagi-stop-emacs ()
  (interactive)
  (if (daemonp)
      (save-buffers-kill-emacs)
    (save-buffers-kill-terminal)))

(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

;; column-number in mode-line.
(column-number-mode 1)

;; Environment variables
;;
;; grep matches with background yellow and foreground black
(setenv "GREP_COLORS" "ms=30;43:mc=30;43:sl=01;37:cx=:fn=35:ln=32:bn=32:se=36")

;; For eshell env settings.
(setenv "STARDICT_DATA_DIR" "~/.stardict/dic")
(prefer-coding-system 'utf-8)

;; Save-minibuffer-history
(setq savehist-file "~/.emacs.d/history"
      history-delete-duplicates t)
(setq history-length 100) ; default is 30.
(savehist-mode 1)
