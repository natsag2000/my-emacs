;;; Nagi's Emacs configuration file
;;; -------------------------------
;;
;; shameless stolen from Howard Abrams
;;
;; TODO: one day do it with literal programming

;;; GENERAL SETTINGS
;;; ----------------

;;; Directory location
;;
(defconst na/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))
(defun na/emacs-subdirectory (d) (expand-file-name d na/emacs-directory))

;;; Directory structure
;;
(let* ((subdirs '("elisp" "backups" "snippets" "ac-dict"))
       (fulldirs (mapcar (lambda (d) (na/emacs-subdirectory d)) subdirs)))
  (dolist (dir fulldirs)
    (when (not (file-exists-p dir))
      (message "Make directory: %s" dir)
      (make-directory dir))))

;;; Customization file
;;
;; settings in ui, menu will be saved here
;;
(setq custom-file (expand-file-name "custom.el" na/emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; Setting up the Load Path
;;
;; extra packages not available via the package manager go in my
;; personal stach at: $HOME/.emacs.d/elisp
;;
(add-to-list 'load-path (na/emacs-subdirectory "elisp"))
(add-to-list 'load-path "~/elisp")
(require 'cl)

;;; PACKAGE INITIALIZATION
;;; -----------------------

;;; Package manager
;;
(require 'package)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages")))

(setq package-pinned-packages '((async . "melpa")
                                (magit . "melpa-stable")
                                (magit-popup . "melpa-stable")
                                (org . "org")))
(package-initialize)

;; solution until all converted to use-package
(defun packages-install (packages)
  "Given a list of packages, this will install them from the standard locations."
  (let ((to-install (inverse-filter 'package-installed-p packages)))
    (when to-install
      (package-refresh-contents)
      (dolist (it to-install)
        (package-install it)
        (delete-other-windows)))))

;;; Use-Package
;;
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;; VARIABLES
;;; ---------
(setq user-full-name "Natsagdorj Shagdar (nagi)")
(if (equal "Workmachiname" system-name)
    (setq user-mail-address "n.shagdar@dvz-mv.de")
  (setq user-mail-address "natsag2000@gmail.com"))

;;; Tabs vs Spaces
;; Indent-only-with-spaces
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; make tab key to indent first then completion
(setq-default tab-always-indent 'complete)


;;; Encrypting Files
;;
;; on mac install gpg
;; brew install gpg

;; now, any file loaded with a gpg extension will prompt password
(setq epa-file-select-keys 2)
;; you can have emacs cache th password. not sure i do...
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;;; DISPLAY SETTINGS
;;; ----------------
;;
(setq initial-scratch-message "") ;; Uh, I know what Scratch is for
(setq visible-bell t)             ;; Get rid of the beeps
(when (window-system)
  (tool-bar-mode 0)               ;; Toolbars were only cool with XEmacs
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (scroll-bar-mode -1))            ;; Scrollbars are waste screen estate

;;; Mode Line
;;
(require 'init-mode-line)

;;; Whitespace Mode
;;
(use-package whitespace
  :bind ("C-c T w" . whitespace-mode)
  :init
  (setq whitespace-line-column nil
        whitespace-display-mappings '((space-mark 32 [183] [46])
                                      (newline-mark 10 [9166 10])
                                      (tab-mark 9 [9654 9] [92 9])))
  :config
  (set-face-attribute 'whitespace-space       nil :foreground "#666666" :background nil)
  (set-face-attribute 'whitespace-newline     nil :foreground "#666666" :background nil)
  (set-face-attribute 'whitespace-indentation nil :foreground "#666666" :background nil)
  :diminish whitespace-mode)





;; setting PATH for eshell
(setenv "PATH" (concat
                "/home/nagi/opt/browser/firefox" ":"
                (getenv "PATH")))


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
;; (setq display-time-24hr-format t)
;; (setq display-time-day-and-date t)
;; (display-time)
;; (setq display-time-use-mail-icon t)

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
 '(diff-switches "-w")
 '(org-agenda-files
   (quote
    ("/home/nagi/org/personal/notes.org" "/home/nagi/org/personal/tasks.org"))))
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
;; (setq special-display-regexps `(("\\*Help"
;;                                  (minibuffer . nil)
;;                                  (width . 80)
;;                                  (height . 24)
;;                                  (left-fringe . 0)
;;                                  (border-width . 0)
;;                                  (menu-bar-lines . 0)
;;                                  (tool-bar-lines . 0)
;;                                  (unsplittable . t)
;;                                  (top . 24)
;;                                  (left . 450)
;;                                  (background-color . "Lightsteelblue1")
;;                                  (foreground-color . "black")
;;                                  (alpha . nil)
;;                                  (fullscreen . nil))
;;                                 ("\\*Compile-Log"
;;                                  (minibuffer . nil)
;;                                  (width . 85)
;;                                  (height . 24)
;;                                  (left-fringe . 0)
;;                                  (border-width . 0)
;;                                  (menu-bar-lines . 0)
;;                                  (tool-bar-lines . 0)
;;                                  (unsplittable . t)
;;                                  (top . 24)
;;                                  (left . 450)
;;                                  (background-color . "Brown4")
;;                                  (foreground-color . "black")
;;                                  (alpha . nil)
;;                                  (fullscreen . nil))
;;                                 ("\\*Dict"
;;                                  (minibuffer . nil)
;;                                  (width . 80)
;;                                  (height . 24)
;;                                  (left-fringe . 0)
;;                                  (border-width . 0)
;;                                  (menu-bar-lines . 0)
;;                                  (tool-bar-lines . 0)
;;                                  (unsplittable . t)
;;                                  (top . 24)
;;                                  (left . 450)
;;                                  (background-color . "LightSteelBlue")
;;                                  (foreground-color . "DarkGoldenrod")
;;                                  (alpha . nil)
;;                                  (fullscreen . nil))
;;                                 ))

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
;; (add-to-list 'display-time-world-list '("Greenwich" "Greenwich"))
;; (add-to-list 'display-time-world-list '("Australia/Sydney" "Sydney"))
;; (add-to-list 'display-time-world-list '("Australia/Melbourne" "Melbourne"))
;; (add-to-list 'display-time-world-list '("Australia/Canberra" "Canberra"))
;; (add-to-list 'display-time-world-list '("America/Chicago" "Chicago"))
;; (add-to-list 'display-time-world-list '("America/Denver" "Denver"))
;; (add-to-list 'display-time-world-list '("America/Los_Angeles" "Los_Angeles/Seattle"))
;; (add-to-list 'display-time-world-list '("America/Denver" "Moab"))
;; (add-to-list 'display-time-world-list '("America/Vancouver" "Vancouver"))
;; (add-to-list 'display-time-world-list '("America/Montreal" "Montreal"))
;; (add-to-list 'display-time-world-list '("America/New_York" "Ottawa"))
;; (add-to-list 'display-time-world-list '("Europe/Moscow" "Moscow"))
;; (add-to-list 'display-time-world-list '("Europe/Berlin" "Berlin"))
;; (add-to-list 'display-time-world-list '("Europe/Oslo" "Oslo"))
;; (add-to-list 'display-time-world-list '("Europe/Lisbon" "Lisbon"))
;; (add-to-list 'display-time-world-list '("Asia/Dubai" "Dubai"))
;; (add-to-list 'display-time-world-list '("Asia/Tokyo" "Tokyo"))
;; (add-to-list 'display-time-world-list '("Hongkong" "Hongkong"))
;; (add-to-list 'display-time-world-list '("Indian/Antananarivo" "Antananarivo"))
;; (add-to-list 'display-time-world-list '("Indian/Reunion" "Reunion"))


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
;;(use-package org :ensure t :config (use-package org-config-nagi))
(require 'init-org-nagi)

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

;;; Emacs Browser setup
;;
(require 'init-browser-nagi)

;; (use-package w3m
;;   :ensure t
;;   :init (require 'config-w3m)
;;   :bind ("<f7> h" . w3m)
;;   :defer t)

(setq browse-url-browser-function 'helm-browse-url-firefox)
;;(setq browse-url-browser-function 'w3m-browse-url)

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


;;; Auto complete
;;
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :diminish company-mode)
(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1))

;;; Yasnippets
;;
;; org-mode : https://github.com/yyr/yasnippets-orgmode.git
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (na/emacs-subdirectory "snippets")))

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


;;; Block Wrappers
;;
;; While the =M-(= binding to =insert-pair= is great, I often need to
;; wrap with other characters:
(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-{") 'insert-pair)
;;(global-set-key (kbd "M-<") 'insert-pair) ;; conflict with go to start
(global-set-key (kbd "M-'") 'insert-pair)
(global-set-key (kbd "M-`") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)

;;
;; But [[https://github.com/rejeep/wrap-region.el][wrap-region]] is even more flexible.
;; NOTE: just select region and press any wrapping char
(use-package wrap-region
  :ensure   t
  :config
  (wrap-region-global-mode t)
  (wrap-region-add-wrappers
   '(("(" ")")
     ("[" "]")
     ("{" "}")
     ("<" ">")
     ("'" "'")
     ("\"" "\"")
     ("‘" "’"   "q")
     ("“" "”"   "Q")
     ("*" "*"   "b"   org-mode)                 ; bolden
     ("*" "*"   "*"   org-mode)                 ; bolden
     ("/" "/"   "i"   org-mode)                 ; italics
     ("/" "/"   "/"   org-mode)                 ; italics
     ("~" "~"   "c"   org-mode)                 ; code
     ("~" "~"   "~"   org-mode)                 ; code
     ("=" "="   "v"   org-mode)                 ; verbatim
     ("=" "="   "="   org-mode)                 ; verbatim
     ("_" "_"   "u" '(org-mode markdown-mode))  ; underline
     ("**" "**" "b"   markdown-mode)            ; bolden
     ("*" "*"   "i"   markdown-mode)            ; italics
     ("`" "`"   "c" '(markdown-mode ruby-mode)) ; code
     ("`" "'"   "c"   lisp-mode)                ; code
     ))
  :diminish wrap-region-mode)


(defun surround (start end txt)
  "Wraps the specified region (or the current 'symbol / word'
     with some textual markers that this function requests from the
     user. Opening-type text, like parens and angle-brackets will
     insert the matching closing symbol.

     This function also supports some org-mode wrappers:

       - `#s` wraps the region in a source code block
       - `#e` wraps it in an example block
       - `#q` wraps it in an quote block"
  (interactive "r\nsEnter text to surround: " start end txt)

  ;; If the region is not active, we use the 'thing-at-point' function
  ;; to get a "symbol" (often a variable or a single word in text),
  ;; and use that as our region.

  (if (not (region-active-p))
      (let ((new-region (bounds-of-thing-at-point 'symbol)))
        (setq start (car new-region))
        (setq end (cdr new-region))))

  ;; We create a table of "odd balls" where the front and the end are
  ;; not the same string.
  (let* ((s-table '(("#e" . ("#+BEGIN_EXAMPLE\n" "\n#+END_EXAMPLE") )
                    ("#s" . ("#+BEGIN_SRC \n"    "\n#+END_SRC") )
                    ("#q" . ("#+BEGIN_QUOTE\n"   "\n#+END_QUOTE"))
                    ("<"  . ("<" ">"))
                    ("("  . ("(" ")"))
                    ("{"  . ("{" "}"))
                    ("["  . ("[" "]"))))    ; Why yes, we'll add more
         (s-pair (assoc-default txt s-table)))

    ;; If txt doesn't match a table entry, then the pair will just be
    ;; the text for both the front and the back...
    (unless s-pair
      (setq s-pair (list txt txt)))

    (save-excursion
      (narrow-to-region start end)
      (goto-char (point-min))
      (insert (car s-pair))
      (goto-char (point-max))
      (insert (cadr s-pair))
      (widen))))

(global-set-key (kbd "C-+") 'surround)

(defun surround-text (txt)
  (if (region-active-p)
      (surround (region-beginning) (region-end) txt)
    (surround nil nil txt)))

(defun surround-text-with (surr-str)
  "Returns an interactive function that when called, will surround the region (or word) with the SURR-STR string."
  (lexical-let ((text surr-str))
    (lambda ()
      (interactive)
      (surround-text text))))


;;; Grep for my Notes
;;
;; on CLI
;; install on linux
;; sudo apt-get install silversearcher-ag
;; install on mac
;;   brew tap homebrew/dupes
;;   brew install homebrew/dupes/grep
;;
;; on Emacs
;; Silver searcher
;;
;; - =ag-project-at-point= :: sets the query with the word at point, use: =C-c p s s=
;; - =ag-regexp= :: searches for regular expressions in a chosen
;;                  directory (*Note:* the =ag= command prompts with
;;                  =regexp=, but it adds a =--literal= option to the command)
;; - =C-u= :: Adding a prefix adds command line options, like =-s= or
;;            =-i= to specify case-sensitivity.
;; #+BEGIN_SRC org :tangle ~/.agignore
;;   #.*
;; #+END_SRC
(use-package ag
  :ensure    t
  :commands  ag
  :init      (setq ag-highlight-search t)
  :config    (add-to-list 'ag-arguments "--word-regexp"))

(use-package helm-ag
  :ensure    t)

;;; Recent files list
;;
;; Emacs already has the recent file
;; listing available, just not turned on.
(use-package recentf
  :init
  (setq recentf-max-menu-items 25)
  (setq recentf-auto-cleanup 'never)
  ;; (setq recentf-keep '(file-remote-p file-readable-p))
  (recentf-mode 1)
  :bind ("C-c f f" . recentf-open-files))

;;; Line Numbers
;;
(add-hook 'prog-mode-hook 'linum-mode)
;; If we make the line numbers a fixed size, then increasing or
;;    decreasing the font size doesn't truncate the numbers:
(defun fix-linum-size ()
  (interactive)
  (set-face-attribute 'linum nil :height 110))
(add-hook 'linum-mode-hook 'fix-linum-size)

;; If we alternate between line numbers and no-line numbers, I also
;;    have to turn on/off the fringe. Actually, this is really only
;;    useful when giving presentations.
(defun linum-off-mode ()
  "Toggles the line numbers as well as the fringe. This allows me
     to maximize the screen estate."
  (interactive)
  (if linum-mode
      (progn
        (fringe-mode '(0 . 0))
        (linum-mode -1))
    (fringe-mode '(8 . 0))
    (linum-mode 1)))

(global-set-key (kbd "A-C-K") 'linum-off-mode)
(global-set-key (kbd "s-C-K") 'linum-off-mode)  ;; For Linux

;; I'm intrigued with the [[https://github.com/coldnew/linum-relative][linum-relative]] mode (especially since I can
;;    toggle between them). The idea is that I can see the line that I
;;    want to jump to (like one 9 lines away), and then =C-9 C-n= to
;;    quickly pop to it.

(use-package linum-relative
  :ensure t
  :config
  ;; Otherwise, let's take advantage of the relative line numbering:
  (defun linum-new-mode ()
    "If line numbers aren't displayed, then display them.
          Otherwise, toggle between absolute and relative numbers."
    (interactive)
    (if linum-mode
        (linum-relative-toggle)
      (linum-mode 1)))

  :bind ("A-k" . linum-new-mode)
  ("s-k" . linum-new-mode))   ;; For Linux

;;; Flycheck
;;
(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;;; PROGRAMMING

;;; Tag support

;; All programming languages require some sort of tagging. but after
;;    thirty years, we are still using good ol’ ctags...well,
;;    [[http://ctags.sourceforge.net][Exuberant Ctags]].   Install with Homebrew:

;;    #+BEGIN_SRC sh :tangle no
;;      brew install --HEAD ctags
;;    #+END_SRC

;;    On Ubuntu Linux, do:

;;    #+BEGIN_SRC sh :tangle no
;;      sudo apt-get install -y exuberant-ctags
;;    #+END_SRC

;;    Note: for every project, run the following command:

;;    #+BEGIN_SRC sh :tangle no
;;      ctags -e -R .
;;    #+END_SRC

;;    I want to be able to add headers from my =org-mode= files as
;;    a /language option/:

;;    #+BEGIN_SRC sh :tangle ~/.ctags :comments no
;;     --langdef=org
;;     --langmap=org:.org
;;     --regex-org=/^\*+[ \t]+([a-zA-Z0-9_ ]+)/\1/d,definition/
;;    #+END_SRC

;;    We access stuff by loading the =etags= package:

(require 'etags)

;; Now, use the following keys:

;;    - M-. :: To find the tag at point to jump to the function’s
;;             definition when the point is over a function call. It is a
;;             dwim-type function.
;;    - M-, :: jump back to where you were.
;;    - M-? :: find a tag, that is, use the Tags file to look up a
;;             definition. If there are multiple tags in the project with
;;             the same name, use `C-u M-.’ to go to the next match.
;;    - =M-x tags-search= :: regexp-search through the source files
;;         indexed by a tags file (a bit like =grep=)
;;    - =M-x tags-query-replace= :: query-replace through the source files
;;         indexed by a tags file
;;    - =M-x tags-apropos= :: list all tags in a tags file that match a
;;         regexp
;;    - =M-x list-tags= :: list all tags defined in a source file

;;    With the fancy new [[https://marmalade-repo.org/packages/ctags-update][ctags-update]] package, we can update the tags file
;;    whenever we save a file:
(use-package ctags-update
  :ensure t
  :config
  (add-hook 'prog-mode-hook  'turn-on-ctags-auto-update-mode)
  :diminish ctags-auto-update-mode)

;;And if I'm lazy and willing to use the mouse:
(use-package imenu+
  :ensure t
  :init (add-hook 'prog-mode-hook 'imenup-add-defs-to-menubar)
  (add-hook 'org-mode-hook  'imenup-add-defs-to-menubar))

;;If I don't know what I'm after, Helm is better:
(use-package helm
  :bind (("C-c M-i" . helm-imenu)))

(use-package guide-key
       :init (add-to-list 'guide-key/guide-key-sequence "C-x c"))

;; Emacs 25 changed has now deprecated the famous [[info:emacs#Tags][Tags and Friends]],
;;    like =find-tags= for =xref=. Some point, I will have to learn how
;;    to configure it, but until then, I'll just rebind to my old mates:

(global-set-key (kbd "M-.") 'find-tag)
(global-set-key (kbd "C-M-.") 'find-tag-regexp)
(global-set-key (kbd "M-,") 'pop-tag-mark)
(global-set-key (kbd "M-i") 'imenu-anywhere)

;;Note: This prompt needs to go away:
(setq tags-add-tables nil)

;;; Code Block Folding

;; The [[info:emacs#Hideshow][Hide Show Minor]] mode allows us to /fold/ all functions
;;     (hidden), showing only the header lines. We need to turn on the
;;     mode, so wrappers are in order:
(defun ha/hs-show-all ()
  (interactive)
  (hs-minor-mode 1)
  (hs-show-all))

(defun ha/hs-hide-all ()
  (interactive)
  (hs-minor-mode 1)
  (hs-hide-all))

(defun ha/hs-toggle-hiding ()
  (interactive)
  (hs-minor-mode 1)
  (hs-toggle-hiding))

;; Seems that =C-c @= is too obnoxious to use, so I'll put my
;;     favorite on the =C-c h= prefix:
(use-package hs-minor-mode
  :bind
  ("C-c T h" . hs-minor-mode)
  ("C-c h a" . ha/hs-hide-all)
  ("C-c h s" . ha/hs-show-all)
  ("C-c h h" . ha/hs-toggle-hiding))

;;; Web
;;
(require 'init-nagi-web)

;;; js2
;;
(require 'init-javascript-nagi)

;;; using Dash
(if (eq system-type 'darwin)
    (require 'init-mac-dash)
  (require 'init-linux-dash))
