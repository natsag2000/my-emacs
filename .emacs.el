(setq user-full-name "Natsagdorj Shagdar (nagi)")
(setq user-mail-address "")

(require 'cl-lib)

;; setting PATH for eshell
;; (setenv "PATH" (concat
;;                 "/home/nagi/.nvm" ":"
;;                 (getenv "PATH")))



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
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-safe-themes
   (quote
    ("9ff70d8009ce8da6fa204e803022f8160c700503b6029a8d8880a7a78c5ff2e5" "5fa16199974646cc61ecec63b315701ad589aa28dfca282174e3fdd818b81d9d" default)))
 '(diff-switches "-w"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


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

;;; Powerline
;; testing
;; (use-package powerline
;;        :ensure t)
;; (custom-set-faces
;;      '(mode-line-buffer-id ((t (:foreground "#000000" :bold t))))
;;      '(which-func ((t (:foreground "#77aaff"))))
;;      '(mode-line ((t (:foreground "#000000" :background "#dddddd" :box nil))))
;;      '(mode-line-inactive ((t (:foreground "#000000" :background "#bbbbbb" :box nil)))))


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
  :ensure t
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

(use-package dired-async
  :config (dired-async-mode 1))

(use-package async-bytecomp
  :config (setq async-bytecomp-allowed-packages '(all)))

;;; Helm
;;
(use-package helm
  :ensure t
  :init (load "init-helm-nagi.el"))


;; Zoom-window
;;
(use-package zoom-window
  :ensure t
  :init (setq zoom-window-mode-line-color "DarkGreen")
  :bind ("C-x C-z" . zoom-window-zoom))


;;; Info
;;
(use-package info
  :init
  (progn
    ;; Additional info directories
    (add-to-list 'Info-directory-list "/usr/local/share/info")
    (add-to-list 'Info-directory-list "/usr/share/info")
    (add-to-list 'Info-directory-list "~/elisp/info")
    (add-to-list 'Info-directory-list "~/elisp/info/eshell-doc")
    ;; Fancy faces in info.
    (defface tv-info-ref-item
      '((((background dark)) :background "DimGray" :foreground "Gold")
        (((background light)) :background "firebrick" :foreground "LightGray"))
      "Face for item stating with -- in info." :group 'Info :group 'faces)

    (defvar tv-info-title-face 'tv-info-ref-item)
    (defvar tv-info-underline 'underline)
    (defvar info-unicode-quote-start (string 8216))
    (defvar info-unicode-quote-end (string 8217))
    (defvar info-unicode-quoted-regexp (format "[%s]\\([^%s%s]+\\)[%s]"
                                               info-unicode-quote-start
                                               info-unicode-quote-start
                                               info-unicode-quote-end
                                               info-unicode-quote-end
                                               ))
    (defun tv-font-lock-doc-rules ()
      (font-lock-add-keywords
       nil `(("[^\\s\][`]\\([^`']+\\)[`']?[^\\s\][']?" 1 font-lock-type-face)
             (,info-unicode-quoted-regexp 1 font-lock-type-face)
             ("^ --.*$" . tv-info-title-face)
             ("[_]\\([^_]+\\)[_]" 1 tv-info-underline)
             ("[\"]\\([^\"]*\\)[\"]" . font-lock-string-face)
             ("\\*Warning:\\*" . font-lock-warning-face)
             ("^ *\\([*•]\\) " 1 font-lock-variable-name-face)
             ("^[[:upper:],]\\{2,\\}$" . font-lock-comment-face)
             ("^[[:upper]][a-z- ]*:" . font-lock-variable-name-face)
             )))

    (add-hook 'Info-mode-hook 'tv-font-lock-doc-rules)))

;;; emacs-wget site-lisp configuration
;;
;;
(autoload 'wget "wget" "wget interface for Emacs." t)
(autoload 'wget-web-page "wget" "wget interface to download whole web page." t)

(use-package w3m-wget)
;; Use wget in eshell.
(defun eshell/wget (url)
  (wget url))


;;; Firefox protocol
;;
(autoload 'firefox-protocol-installer-install "firefox-protocol" nil t)

;;; Org
;;
(use-package org :ensure t :config (use-package org-config-nagi))

;; ace-jump-mode
(use-package ace-jump-mode
  :ensure t
  :init
  (define-key global-map (kbd "C-0") 'ace-jump-mode))

;; expand-region
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

;;; W3m
;;
(use-package w3m
  :ensure t
  :init (require 'config-w3m)
  :bind ("<f7> h" . w3m)
  :defer t)

;;; FUN GAME
;;
(use-package pacmacs :ensure t)

;; 2048-game
(use-package 2048-game :ensure t)

;;; Emms
;;
(use-package emms
  :ensure t
  :config (use-package emms-vlc-config)
  :commands (emms-stream-init))

;;; Magit
;;
(use-package magit
  :init
  (progn
    (setq magit-status-buffer-name-format "*magit status: %a*")
    (setq magit-restore-window-configuration t)
    (setq git-commit-fill-column 120)
    (setq git-commit-summary-max-length 80)
    (setq auto-revert-verbose nil)
    (setq magit-revision-show-gravatars nil)
    (setq magit-uniquify-buffer-names nil))
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (bind-key "C" 'magit-commit-add-log magit-diff-mode-map)
  (bind-key "C-]" 'magit-toggle-margin magit-log-mode-map)
  :no-require t
  :ensure t)

;;; linum-relative
;;
(use-package linum-relative
  :ensure t
  :commands (linum-relative-mode
             helm-linum-relative-mode))

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
                                    ;; Allow yanking right now instead of returning "Mark set"
                                    (push-mark)))

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
      (dolist (i '("tmux" "htop" "alsamixer" "git-log"))
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



;;; Projectile
;; http://tuhdo.github.io/helm-projectile.html
;;
(use-package projectile
  :load-path "site-lisp/projectile"
  :ensure t
  :diminish projectile-mode
  :commands projectile-global-mode
  :defer 5
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (use-package helm-projectile
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on))
  (projectile-global-mode))

(use-package helm-projectile
  :defer t :ensure t
  :ensure helm-projectile)


;;; Bookmark+
;;
(use-package bookmark+ :ensure t)

;;; guide-key
;;
(use-package guide-key
       :ensure t
       :init    (setq guide-key/guide-key-sequence
                      '("C-x r"     ; rectanges and registers
                        "C-x 4"     ; window commands
                        "M-s h"     ; hi-lock highlighting
                        "C-x w"     ; alternative to M-s ...
                        "C-c @"     ; hs-hide-show mode
                        "C-c p"     ; projectile
                        "C-x j"     ; bookmark+
                        "C-x C-z"   ; Zoom window
                        "C-x"       ; general
                        "<f2>"
                        "<f9>"
                        (org-mode "C-c C-x")))
       :config  (guide-key-mode 1)
       :diminish guide-key-mode)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/highlight-command-regexp
      '("rectangle"
        ("register" . font-lock-type-face)
        ("bookmark" . "hot pink")))

;;; fancy-narrow
(use-package fancy-narrow
  :ensure t
  :config
  (defun ha/highlight-block ()
    "Highlights a 'block' in a buffer defined by the first blank
     line before and after the current cursor position. Uses the
          'fancy-narrow' mode to high-light the block."
    (interactive)
    (let (cur beg end)
      (setq cur (point))
      (setq end (or (re-search-forward  "^\s*$" nil t) (point-max)))
      (goto-char cur)
      (setq beg (or (re-search-backward "^\s*$" nil t) (point-min)))
      (fancy-narrow-to-region beg end)
      (goto-char cur)))

  (defun ha/highlight-section (num)
    "If some of the buffer is highlighted with the `fancy-narrow'
          mode, then un-highlight it by calling `fancy-widen'.

          If region is active, call `fancy-narrow-to-region'.

          If NUM is 0, highlight the current block (delimited by blank
          lines). If NUM is positive or negative, highlight that number
          of lines.  Otherwise, called `fancy-narrow-to-defun', to
          highlight current function."
    (interactive "p")
    (cond
     ((fancy-narrow-active-p)  (fancy-widen))
     ((region-active-p)        (fancy-narrow-to-region (region-beginning) (region-end)))
     ((= num 0)                (ha/highlight-block))
     ((= num 1)                (fancy-narrow-to-defun))
     (t                        (progn (ha/expand-region num)
                                      (fancy-narrow-to-region (region-beginning) (region-end))))))

       :bind ("C-M-+" . ha/highlight-section))

;;; endless parentheses
;;
(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
     Intelligently means: region, subtree, or defun, whichever applies
     first.

     With prefix P, don't widen, just narrow even if buffer is already
     narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
        (t (narrow-to-defun))))
(global-set-key (kbd "C-x n x") 'narrow-or-widen-dwim)

;;; ace-window
;;
(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?o))
  (global-set-key (kbd "C-x o") 'ace-window)
  :diminish ace-window-mode)

;;; avy
;;
(use-package avy
  :ensure t
  :commands avy-goto-word-1 avy-goto-char-1 avy-goto-line avy-goto-char-timer
  :bind
  ("C-c j"   . avy-goto-word-1)
  ("C-c k k" . avy-goto-char-timer)
  ("C-c k j" . avy-goto-word-1)
  ("C-c k c" . avy-goto-char-1)
  ("C-c k l" . avy-goto-line))

;; smarter move
;; TODO: create emacs-fixes.el
(defun smarter-move-beginning-of-line (arg)
    "Move point back to indentation of beginning of line.

  Move point to the first non-whitespace character on this line.
  If point is already there, move to the beginning of the line.
  Effectively toggle between the first non-whitespace character and
  the beginning of the line.

  If ARG is not nil or 1, move forward ARG - 1 lines first.  If
  point reaches the beginning or end of the buffer, stop there."
    (interactive "^p")
    (setq arg (or arg 1))

    ;; Move lines first
    (when (/= arg 1)
      (let ((line-move-visual nil))
        (forward-line (1- arg))))

    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (move-beginning-of-line 1))))
;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)
(global-set-key [remap org-beginning-of-line]  'smarter-move-beginning-of-line)

;;; Hydra
;;
(use-package hydra
  :ensure t
  :config
  (hydra-add-font-lock))

;;; Multiple cursors
;;
(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key
   (kbd "C-c C-.")
   (defhydra hydra-multiple-cursors ()
     "multiple-cursors"
     ("." mc/mark-all-dwim                   "all-dwim")
     ("C-." mc/mark-all-like-this-dwim       "all-like-dwim")
     ("n" mc/mark-next-like-this             "next")
     ("p" mc/mark-previous-like-this         "previous")
     ("a" mc/mark-all-like-this              "mark-all")
     ("N" mc/mark-next-symbol-like-this      "next-symbol")
     ("P" mc/mark-previous-symbol-like-this  "previous-symbol")
     ("A" mc/mark-all-symbols-like-this      "all-symbols")
     ("f" mc/mark-all-like-this-in-defun     "in-func")
     ("l" mc/edit-lines                      "all-lines")
     ("e" mc/edit-ends-of-lines              "end-lines"))))


;;; Dired details
;;
(setq ls-lisp-use-insert-directory-program nil)
(use-package dired-details
       :ensure t
       :init   (setq dired-details-hidden-string "* ")
       :config (dired-details-install))

(use-package find-dired
        :ensure t
        :init (setq find-ls-option '("-print0 | xargs -0 ls -od" . "-od")))

(require 'dired-x)

;; TODO group it some where
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Web
;;
(require 'init-nagi-web)

;;; Development
;;
;;; js2
;;
(require 'init-javascript-nagi)

;;; using Dash
(if (eq system-type 'darwin)
    (require 'init-mac-dash)
  (require 'init-linux-dash))

;; Bindings
;;
;; disable alt+space shortcut on unity in settings->keyboard->shortcuts-Windows
;;
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-z")                        nil) ; Disable `suspend-frame'.
(global-set-key (kbd "C-!")                        'eshell-command)
(global-set-key (kbd "C-c R")                      #'(lambda () (interactive) (revert-buffer t t)))
(global-set-key (kbd "C-c W")                      'whitespace-mode)
(global-set-key (kbd "C-M-j")                      #'(lambda () (interactive) (kill-sexp -1)))
