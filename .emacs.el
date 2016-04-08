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

;; Themes
(defvar nagi-theme-directory "~/.emacs.d/themes/naquadah-theme")
(unless (< emacs-major-version 24)
  (setq custom-theme-directory nagi-theme-directory))

;; Load my favourite theme.
;; under ~/.emacs.d/themes, git https://github.com/jd/naquadah-theme.git
(add-hook 'emacs-startup-hook #'(lambda () (load-theme 'naquadah)))
(custom-set-variables
 '(custom-safe-themes
   (quote
    ("9ff70d8009ce8da6fa204e803022f8160c700503b6029a8d8880a7a78c5ff2e5" "5fa16199974646cc61ecec63b315701ad589aa28dfca282174e3fdd818b81d9d" default))))
(custom-set-faces )


;;; Frame and window config.
;;
;;
;; My current-font: [EVAL]: (assoc-default 'font (frame-parameters))
;; Choose a font:   [EVAL]: (progn (when (require 'helm-font) (helm 'helm-source-xfonts)))
;; Choose a color:  [EVAL]: (progn (when (require 'helm-color) (helm 'helm-source-colors)))
;; To reload .Xresources [EVAL]: (shell-command xrdb "~/.Xresources")

(defvar tv-default-font (assoc-default 'font (frame-parameters)))
(setq-default frame-background-mode 'dark)
(setq initial-frame-alist '((fullscreen . maximized)))
(setq frame-auto-hide-function 'delete-frame)

(if (or (daemonp)
        (not (window-system))
        (< emacs-major-version 24))
    (setq default-frame-alist `((vertical-scroll-bars . nil)
                                (tool-bar-lines . 0)
                                (menu-bar-lines . 0)
                                (title . ,(format "Emacs-%s" emacs-version))
                                (font . "-unknown-DejaVu Sans Mono-bold-normal-normal-*-14-*-*-*-m-0-iso10646-1")
                                (cursor-color . "red")))

    (setq default-frame-alist `((foreground-color . "Wheat")
                                (background-color . "black")
                                (alpha . 90)
                                ;; New frames go in right corner.
                                (left . ,(- (* (window-width) 8) 160)) ; Chars are 8 bits long.
                                (vertical-scroll-bars . nil)
                                (title . ,(format "Emacs-%s" emacs-version))
                                (tool-bar-lines . 0)
                                (menu-bar-lines . 0)
                                (font . ,tv-default-font)
                                (cursor-color . "red")
                                (fullscreen . nil)
                                )))

;; Speedbar
(add-hook 'speedbar-load-hook
          #'(lambda ()
              (setq speedbar-frame-parameters
                    `((minibuffer . nil)
                      (font . ,tv-default-font)
                      (width . 20)
                      (fullscreen . nil) ; Not needed when fullscreen isn't set in .Xressources.
                      (left . ,(- (* (window-width) 8)
                                  (frame-width))) ; Speed-bar on right of screen.
                      (border-width . 0)
                      (menu-bar-lines . 0)
                      (tool-bar-lines . 0)
                      (unsplittable . t)
                      (left-fringe . 0)))))

;;; Special buffer display.
;;
;;
(setq special-display-regexps `(("\\*Help"
                                 (minibuffer . nil)
                                 (width . 80)
                                 (height . 24)
                                 (left-fringe . 0)
                                 (border-width . 0)
                                 (menu-bar-lines . 0)
                                 (tool-bar-lines . 0)
                                 (unsplittable . t)
                                 (top . 24)
                                 (left . 450)
                                 (background-color . "Lightsteelblue1")
                                 (foreground-color . "black")
                                 (alpha . nil)
                                 (fullscreen . nil))
                                ("\\*Compile-Log"
                                 (minibuffer . nil)
                                 (width . 85)
                                 (height . 24)
                                 (left-fringe . 0)
                                 (border-width . 0)
                                 (menu-bar-lines . 0)
                                 (tool-bar-lines . 0)
                                 (unsplittable . t)
                                 (top . 24)
                                 (left . 450)
                                 (background-color . "Brown4")
                                 (foreground-color . "black")
                                 (alpha . nil)
                                 (fullscreen . nil))
                                ("\\*Dict"
                                 (minibuffer . nil)
                                 (width . 80)
                                 (height . 24)
                                 (left-fringe . 0)
                                 (border-width . 0)
                                 (menu-bar-lines . 0)
                                 (tool-bar-lines . 0)
                                 (unsplittable . t)
                                 (top . 24)
                                 (left . 450)
                                 (background-color . "LightSteelBlue")
                                 (foreground-color . "DarkGoldenrod")
                                 (alpha . nil)
                                 (fullscreen . nil))
                                ))

;; Don't split this windows horizontally
(setq split-width-threshold nil)

;; Pas-de-dialog-gtk
(setq use-file-dialog nil)

;; Browse url
;;
;;
(setq browse-url-browser-function 'helm-browse-url-firefox)
;;(setq browse-url-browser-function 'w3m-browse-url)

;;; Ediff
;;
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;;; emacs-backup-config
;;
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs_backup"))
      backup-by-copying t
      version-control t
      kept-old-versions 2
      kept-new-versions 20
      delete-old-versions t)
(setq tramp-backup-directory-alist backup-directory-alist)

;; show-paren-mode
;;
(show-paren-mode 1)
(setq show-paren-ring-bell-on-mismatch t)

;; Start-emacs-server
;;
(add-hook 'after-init-hook #'(lambda ()
                               (unless (daemonp)
                                 (server-start)
                                 (setq server-raise-frame t))))

;; Path-to-abbrev-file
;;(setq abbrev-file-name "/home/thierry/.emacs.d/.abbrev_defs")

;; Copy/paste
(setq select-active-regions t)
(setq x-select-enable-clipboard-manager nil)

;; Enable-commands-disabled-by-default
(put 'narrow-to-region 'disabled nil) ; C-x n n
(put 'narrow-to-page 'disabled nil)   ; C-x n p
(put 'scroll-left 'disabled nil)     ; C-x > or <
(put 'downcase-region 'disabled nil) ; C-x C-l
(put 'upcase-region 'disabled nil)   ; C-x C-u
(put 'set-goal-column 'disabled nil) ; C-x C-n ==> disable with C-u
(put 'dired-find-alternate-file 'disabled nil) ; a in dired

;; setup-minibuffer
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; Woman/man
(setq woman-use-own-frame nil)
(setq Man-notify-method 'pushy)
(defface man-args-face '((t (:foreground "Magenta" :underline t)))
  "*Face used in man page to show arguments and sections."
  :group 'man)

;; Printing
;; (setq lpr-command "gtklp")
;; (setq printer-name "EpsonStylus")
;; (setq-default ps-print-header nil)
;; (setq ps-font-size   '(10 . 11.5))
;; (setq ps-font-family 'Courier)

;; auto-compression-mode
(auto-compression-mode 1)

;; Mode-lecture-photo-auto
(auto-image-file-mode 1)

;; Allow scrolling horizontally in large images
(add-hook 'image-mode-hook #'(lambda () (set (make-variable-buffer-local 'auto-hscroll-mode) nil)))

;; line-move-visual.
(setq line-move-visual nil)

;; Trash
;; (setq delete-by-moving-to-trash t)

;; Minibuffers completion
(setq completion-cycle-threshold t) ; always cycle, no completion buffers.

;; Diff
(customize-set-variable 'diff-switches "-w")

;; Report bug
(setq report-emacs-bug-no-explanations t)

;; Indent-only-with-spaces
(setq-default indent-tabs-mode nil)

;; Prompt shell read only
(setq comint-prompt-read-only t)

;; Newline and indent in `sh-mode'.
(add-hook 'sh-mode-hook #'(lambda ()
                            (define-key sh-mode-map (kbd "RET") 'newline-and-indent)))

;; winner-mode config
(setq winner-boring-buffers '("*Completions*"
                              "*Compile-Log*"
                              "*inferior-lisp*"
                              "*Fuzzy Completions*"
                              "*Apropos*"
                              "*Help*"
                              "*cvs*"
                              "*Buffer List*"
                              "*Ibuffer*"
                              ))

(winner-mode 1)

;; Display time in mode-line
(setq display-time-string-forms
      '( ;; date
        (if (and (not display-time-format) display-time-day-and-date)
            (format-time-string "[%a %e %b " now)
            "")
        ;; time
        (concat
         (propertize
          (format-time-string (or display-time-format
                                  (if display-time-24hr-format " %H:%M" " %-I:%M%p"))
                              now)
          'face '((:foreground "green"))
          'help-echo (format-time-string " %a %b %e, %Y" now))
         (and time-zone " (") time-zone (and time-zone ")")
         "]")
        ;; cpu load average
        ;; (if (and load (not (string= load "")))
        ;;     (format "cpu:%s" load) "")
        ""
        ;; mail
        ""))

;; Mode-line
(set-face-attribute 'mode-line-emphasis nil :foreground "red")

;; World-time
(add-to-list 'display-time-world-list '("Greenwich" "Greenwich"))
(add-to-list 'display-time-world-list '("Australia/Sydney" "Sydney"))
(add-to-list 'display-time-world-list '("Australia/Melbourne" "Melbourne"))
(add-to-list 'display-time-world-list '("Australia/Canberra" "Canberra"))
(add-to-list 'display-time-world-list '("America/Chicago" "Chicago"))
(add-to-list 'display-time-world-list '("America/Denver" "Denver"))
(add-to-list 'display-time-world-list '("America/Los_Angeles" "Los_Angeles/Seattle"))
(add-to-list 'display-time-world-list '("America/Denver" "Moab"))
(add-to-list 'display-time-world-list '("America/Vancouver" "Vancouver"))
(add-to-list 'display-time-world-list '("America/Montreal" "Montreal"))
(add-to-list 'display-time-world-list '("America/New_York" "Ottawa"))
(add-to-list 'display-time-world-list '("Europe/Moscow" "Moscow"))
(add-to-list 'display-time-world-list '("Europe/Berlin" "Berlin"))
(add-to-list 'display-time-world-list '("Europe/Oslo" "Oslo"))
(add-to-list 'display-time-world-list '("Europe/Lisbon" "Lisbon"))
(add-to-list 'display-time-world-list '("Asia/Dubai" "Dubai"))
(add-to-list 'display-time-world-list '("Asia/Tokyo" "Tokyo"))
(add-to-list 'display-time-world-list '("Hongkong" "Hongkong"))
(add-to-list 'display-time-world-list '("Indian/Antananarivo" "Antananarivo"))
(add-to-list 'display-time-world-list '("Indian/Reunion" "Reunion"))


;; flyspell-aspell
(setq-default ispell-program-name "aspell")
(setq ispell-local-dictionary "francais")

;; Kill buffer after C-d in ansi-term.
(defadvice term-sentinel (after kill-buffer activate)
  (kill-buffer))

;; Require with messages to debug more easily.
(defun tv-require (feature &optional filename noerror)
  (message "Loading %s..." (symbol-name feature))
  (condition-case err
      (if (require feature filename noerror)
          (message "Loading %s done" (symbol-name feature))
          (message "Loading %s Failed" (symbol-name feature)))
    (error
     (signal 'error (list feature (car err) (cadr err))))))

;;; load-path
;;
(dolist (i '("/usr/local/share/emacs/site-lisp"
             "/usr/local/share/emacs/site-lisp/mu4e"
	     "~/elisp/"
             "~/elisp/google-maps.el"
             "~/elisp/Emacs-wgrep"
             "~/elisp/auctex"
             "~/elisp/auctex/preview"
	     "~/elisp/autoconf-mode"
	     "~/elisp/desktop-file-utils"
	     "~/elisp/emacs-wget"
	     "~/elisp/tex-utils"
	     "~/elisp/ledger/"
             "~/elisp/helm"
             "~/elisp/helm-extensions"
             "~/.emacs.d/themes/"
	     "~/.emacs.d/emacs-config/"
	     ))
  ;; Add all at end of `load-path' to avoid conflicts.
  (add-to-list 'load-path (file-name-as-directory i) t))

;;; Async
;;
(use-package async
    :config
  (progn
    (defun tv/async-byte-compile-file (file)
      (interactive "fFile: ")
      (let ((proc
             (async-start
              `(lambda ()
                 (require 'bytecomp)
                 ,(async-inject-variables "\\`load-path\\'")
                 (let ((default-directory ,(file-name-directory file)))
                   (add-to-list 'load-path default-directory)
                   (byte-compile-file ,file))))))

        (unless (condition-case err
                    (async-get proc)
                  (error (ignore (message "Error: %s" (car err)))))
          (message "Recompiling %s...FAILED" file))))))

(use-package dired-async :config (dired-async-mode 1))
(use-package smtpmail-async
    :commands 'async-smtpmail-send-it)
(use-package async-bytecomp
    :config (setq async-bytecomp-allowed-packages '(all)))

;;; Helm
;;
(use-package helm
  :ensure t
  :init (load "init-helm-nagi.el"))


;; Zoom-window
;;
;; (use-package zoom-window
;;   :ensure t
;;     :init (setq zoom-window-mode-line-color "DarkGreen")
;;     :bind ("C-x C-z" . zoom-window-zoom))


;; TODO: 
(global-set-key (kbd "C-x C-j") 'dired-jump)
