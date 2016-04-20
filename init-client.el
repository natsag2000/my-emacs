;; The settings in this file are only included on a Macintosh, Linux, or
;; other systems with a graphical front-end.

;;; Key Bindings
;;

;; confirm-quit-emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; I would like Command-W to close a frame, but only if it only has a
;;   single window in it. I found this code on [[http://www.emacswiki.org/emacs/frame-cmds.el][this site]].
(defun delete-single-window (&optional window)
  "Remove WINDOW from the display.  Default is `selected-window'.
  If WINDOW is the only one in its frame, then `delete-frame' too."
  (interactive)
  (save-current-buffer
    (setq window (or window (selected-window)))
    (select-window window)
    (kill-buffer)
    (if (one-window-p t)
        (delete-frame)
      (delete-window (selected-window)))))


;; Kill emacs
(defun nagi-stop-emacs ()
  (interactive)
  (if (daemonp)
      (save-buffers-kill-emacs)
    (save-buffers-kill-terminal)))


;;; Font Settings
;;

;; I love syntax highlighting.
(global-font-lock-mode 1)

;; Am I really a monospace font slut? I think so. I keep changing my
;;   font based on the monospace du jour... While I [[http://mplus-fonts.sourceforge.jp/mplus-outline-fonts/download/index.html][M+]] because it is
;;   thinner and has more white space between lines, but [[http://blogs.adobe.com/typblography/2012/09/source-code-pro.html][Source Code Pro]]
;;   is so attractive, oh, and then there is Anonymous Pro...

;;   While thicker, [[https://github.com/tonsky/FiraCode][Fira]] does symbol ligatures. However, [[https://github.com/i-tu/Hasklig][Hasklig]] is a
;;   nice font that is thinner and easier to read, with /some/ symbolic
;;   ligatures that doesn't interfere with my org-mode header bullets.
;; NOTE: evaluate and check the fonts! if nil, install them
(defvar ha/fixed-font-family
  (cond ((x-list-fonts "Hasklig")         "Hasklig")
        ((x-list-fonts "Source Code Pro") "Source Code Pro")
        ((x-list-fonts "Anonymous Pro")   "Anonymous Pro")
        ((x-list-fonts "M+ 1mn")          "M+ 1mn"))
  "My fixed width font based on what is installed, `nil' if not defined.")

;; With the font name situated, I just need to use that to set the
;;   three magic frame settings:
(when ha/fixed-font-family
  (set-frame-font ha/fixed-font-family)
  (set-face-attribute 'default nil :font ha/fixed-font-family :height 110)
  (set-face-font 'default ha/fixed-font-family))

;; Since the headers are based on Adobe’s open source font pair of the
;;   proportional font, [[https://github.com/adobe-fonts/source-sans-pro/releases/tag/2.010R-ro/1.065R-it][Source Sans Pro]], will match the non-proportional
;;   font, [[https://github.com/adobe-fonts/source-code-pro/][Source Code Pro]].

(defvar ha/variable-font-tuple
  (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
        ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
        ((x-list-fonts "Verdana")         '(:font "Verdana"))
        ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
        (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro.")))
  "My variable width font available to org-mode files and whatnot.")


;;; Color Theme
;;

 ;; Use the color theme project by following [[http://www.nongnu.org/color-theme/][these instructions]].
 ;;  We now can do =M-x color-theme-<TAB> RET=
(use-package color-theme
  :ensure t
  :init (require 'color-theme)
  :config (use-package color-theme-sanityinc-tomorrow
            :ensure t))

;; The color themes work quite well, except they don't know about the
;;   org-mode source code blocks, so we need to set up a couple
;;   functions that we can use to set them.

(defun org-src-color-blocks-light ()
  "Colors the block headers and footers to make them stand out more for lighter themes"
  (interactive)
  (custom-set-faces
   '(org-block-begin-line
     ((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF"))))
   '(org-block-background
     ((t (:background "#FFFFEA"))))
   '(org-block
     ((t (:background "#FFFFEA"))))
   '(org-block-end-line
     ((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF"))))

   '(mode-line-buffer-id ((t (:foreground "#005000" :bold t))))
   '(which-func ((t (:foreground "#008000"))))))

(defun org-src-color-blocks-dark ()
  "Colors the block headers and footers to make them stand out more for dark themes"
  (interactive)
  (custom-set-faces
   '(org-block-begin-line
     ((t (:foreground "#008ED1" :background "#002E41"))))
   '(org-block-background
     ((t (:background "#000000"))))
   '(org-block
     ((t (:background "#000000"))))
   '(org-block-end-line
     ((t (:foreground "#008ED1" :background "#002E41"))))

   '(mode-line-buffer-id ((t (:foreground "black" :bold t))))
   '(which-func ((t (:foreground "green"))))))

;; No matter, the theme, I like /some/ of the ideas in the [[https://github.com/jonnay/emagicians-starter-kit/blob/master/themes/org-beautify-theme.org][EMagicians Starter Kit]],
;;   particularly in how the headers are larger, instead of different
;;   colors.
(deftheme ha/org-theme "Sub-theme to beautify org mode")

;; Since I’m using the Powerline project, switching my Emacs color
;;   theme, requires me to call =powerline-reset= in order to get the
;;   colors to apply to the mode line.

;;   We put all of these requirements in a single function call:

(defun ha/change-theme (theme org-block-style)
  "Changes the color scheme and reset the mode line."
  (funcall theme)
  (powerline-reset)
  (funcall org-block-style)

  (let* ((ha/fixed-font-tuple (list :font ha/fixed-font-family))
         (base-font-color     (face-foreground 'default nil 'default))
         (background-color    (face-background 'default nil 'default))
         (primary-color       (face-foreground 'mode-line nil))
         (secondary-color     (face-background 'secondary-selection nil 'region))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
    (custom-theme-set-faces 'ha/org-theme
                            `(org-agenda-structure ((t (:inherit default ,@ha/variable-font-tuple :height 1.5 :underline nil))))
                            `(org-verbatim ((t (:inherit 'fixed-pitched ,@ha/fixed-font-tuple :foreground "#aef"))))
                            `(org-table ((t (:inherit 'fixed-pitched ,@ha/fixed-font-tuple))))
                            `(org-block ((t (:inherit 'fixed-pitched ,@ha/fixed-font-tuple))))
                            `(org-block-background ((t (:inherit 'fixed-pitched ,@ha/fixed-font-tuple))))
                            `(org-block-begin-line ((t (:inherit 'fixed-pitched ,@ha/fixed-font-tuple))))
                            `(org-block-end-line ((t (:inherit 'fixed-pitched ,@ha/fixed-font-tuple))))
                            `(org-level-8 ((t (,@headline ,@ha/variable-font-tuple))))
                            `(org-level-7 ((t (,@headline ,@ha/variable-font-tuple))))
                            `(org-level-6 ((t (,@headline ,@ha/variable-font-tuple))))
                            `(org-level-5 ((t (,@headline ,@ha/variable-font-tuple))))
                            `(org-level-4 ((t (,@headline ,@ha/variable-font-tuple :height 1.1))))
                            `(org-level-3 ((t (,@headline ,@ha/variable-font-tuple :height 1.25))))
                            `(org-level-2 ((t (,@headline ,@ha/variable-font-tuple :height 1.5))))
                            `(org-level-1 ((t (,@headline ,@ha/variable-font-tuple :height 1.75))))
                            `(org-document-title ((t (,@headline ,@ha/variable-font-tuple :height 1.5 :underline nil)))))))

;; And the default startup goes to...night...unless I'm at work, and
;;   then we'll take the bright shiny theme.

(if (equal "nagi-ESPRIMO-P520" system-name)
    (ha/change-theme 'color-theme-sanityinc-tomorrow-day
                     'org-src-color-blocks-light)
  (ha/change-theme 'color-theme-sanityinc-tomorrow-night
                   'org-src-color-blocks-dark))

;; My main reason for wanting to use the color theme project is to
;;   switch between /black on white/ during the day, and /white on
;;   black/ at night. Because I have to pass function references to
;;   my =define-sequence= macro, I use the =list= function call instead of
;;   quoting the list:

(define-sequence 'personal-theme-map "<f9> d" 'ha/change-theme
  (list (list "d" 'color-theme-sanityinc-tomorrow-day      'org-src-color-blocks-light)  ; White on Black
        (list "l" 'color-theme-sanityinc-tomorrow-eighties 'org-src-color-blocks-dark)   ; Lt. Gray on Gray
        (list "m" 'color-theme-sanityinc-tomorrow-bright   'org-src-color-blocks-dark)   ; Bright on Black
        (list "n" 'color-theme-sanityinc-tomorrow-night    'org-src-color-blocks-dark))) ; White on Gray


(provide 'init-client)
