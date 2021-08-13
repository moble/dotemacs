;; Don't open new frame when emacs is called by the OS
(setq ns-pop-up-frames 'nil)

;; Override defaults
(fset 'yes-or-no-p 'y-or-n-p)              ; replace y-e-s by y
(setq inhibit-startup-message t)           ; no splash screen
(setq confirm-kill-emacs 'y-or-n-p)        ; require confirmation before quitting
(setq initial-scratch-message "")          ; blank scratch message at startup
(setq completions-format 'vertical)        ; sort completions vertically
(setq echo-keystrokes 0.1)                 ; don't wait so long to show commands in minibuffer


;; Save history across sessions
(savehist-mode 1)
(defvar savehist-additional-variables)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

;; Don't use dialog boxes for anything (not least because they seem to crash emacs on OS X...)
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice message-box (around prevent-dialog activate)
  "Prevent message-box from activating a dialog"
  (apply #'message (ad-get-args 0)))
(setq use-dialog-box nil)

;; Set font height to be taller and default to Adobe's Source Code Pro
;;(set-face-attribute 'default nil :height 180 :font "Source Code Pro")
;;(set-face-attribute 'default nil :height 180 :font "Iosevka SS02")
;; (set-face-attribute 'default nil :height 180 :font "Fira Code")
(set-face-attribute 'default nil :height 190 :font "JuliaMono")
(set-face-attribute 'mode-line nil :height 160)

;; Set the default frame size to be the full screen height and half the width
(add-to-list 'default-frame-alist (cons 'width 151))
(add-to-list 'default-frame-alist (cons 'height 43))
(setq initial-frame-alist '((top . 0) (left . 0)))

;; ;; Set transparency default
;; (set-frame-parameter (selected-frame) 'alpha '(85 50))
;; (add-to-list 'default-frame-alist '(alpha 85 50))

;; Show line and column numbers
(line-number-mode 1)
(column-number-mode 1)

;; Hide menu, tool bar, and scroll bar
(if (display-graphic-p)
    (progn
      ;; Use mouse wheel
      (mouse-wheel-mode t)

      ;; Slow it down a bit
      (setq mouse-wheel-scroll-amount '(0.03))
      (setq mouse-wheel-progressive-speed nil)

      ;; Skip some ugly things I don't need
      ;;(menu-bar-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      )
  )

;; Better frame titles
(setq frame-title-format (concat  "%b - emacs@" (system-name)))

;; When dragging with the mouse, automatically copy to the clipboard
(setq mouse-drag-copy-region t)

;; Complete with substrings (like ido)
(add-to-list 'completion-styles 'substring)
(setq read-file-name-completion-ignore-case t) ;; ignore case when completing file names
(setq read-buffer-completion-ignore-case t) ;; ignore case when completing buffer names

;; Make tramp use sshx by default
(require 'tramp)
(setq tramp-default-method "sshx")

;; When compiling, keep up with last line of output
(setq compilation-scroll-output t)

;; Helper for compilation. Close the compilation window
;; after exit if there was no error at all.
(defun compilation-exit-autoclose (status code msg)
  (when (and (eq status 'exit) (zerop code))
    (bury-buffer)
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  (cons msg code)
  )
(setq compilation-exit-message-function 'compilation-exit-autoclose)

;; Backup
(defvar backup-dir (expand-file-name (expand-file-name "~/.emacs.d/backup/")))
(defvar autosave-dir (expand-file-name (expand-file-name "~/.emacs.d/autosave/")))
(setq backup-directory-alist (list (cons ".*" backup-dir) (cons tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq backup-by-copying-when-linked t)

;; Retain indentation level when commenting
(setq comment-style 'indent)

;; Use spaces for all indents instead of tabs and show a tab character
;; as "^I".  [Note that `whitespace-cleanup` can do any necessary
;; conversions automatically.]
;; Note that I've added a custom-function named `tolerate-tabs` to turn this off
(setq-default indent-tabs-mode nil)
;; (standard-display-ascii ?\t "^I")

;; Highlight whitespace at the end of a line
(setq show-trailing-whitespace t)

;; Set default "tab" width to 4 spaces, but 2 spaces in c modes
(setq tab-width 4)
(setq c-basic-indent 2)

;; Don't indent preprocessor directives
(c-set-offset (quote cpp-macro) 0 nil)

;; Set the number to the number of columns to use.
(setq-default fill-column 100)

;; Make the bell visible, rather than audible
(setq visible-bell t)

;; Use command as the meta key
(setq mac-command-modifier 'meta)

;; Ignore the alt/option key
(setq mac-option-modifier nil)

;; Just follow git-controlled links without asking
(setq vc-follow-symlinks t)

;; Allow use of C-xC-l and C-xC-u for changing case
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Allow certain variables to be set per-buffer within a file using either the
;; `Local Variables` list (usually at the bottom of the file), or the `-*-`
;; string (usually near the top of the file).
(put 'LocalWords 'safe-local-variable #'stringp) ;; add words to spell-check dictionary for this file
(put 'zotero-collection 'safe-local-variable #'stringp) ;; configure the zotero collection for bibtex
;; See also tex.el

;; Highlight matching paren when on a paren
(show-paren-mode 1)
;; Highlight matching paren when it is visible, otherwise highlight the whole
;; expression
(setq show-paren-style 'mixed)
;; Color matching delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Add /usr/local/bin to the path.  Unfortunately, this seems to be
;; the correct way to give emacs a PATH.  Apparently, PATH is used
;; when you start a shell, whereas exec-path is used by emacs itself
;; when looking for programs.  For example, tramp uses the first
;; version of ssh found in exec-path.
(if (string-equal "darwin" (symbol-name system-type))
    (setenv "PATH" (concat
                    (expand-file-name "~/.bin") ":"
                    "/usr/local/bin:"
                    (getenv "PATH"))))
(setq exec-path
      (append
       `(,(expand-file-name "~/.bin")
         "/usr/local/bin")
       exec-path))
