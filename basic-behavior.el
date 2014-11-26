;; Don't open new frame when emacs is called by the OS
(setq ns-pop-up-frames 'nil)

;; Override defaults
(fset 'yes-or-no-p 'y-or-n-p)           ;replace y-e-s by y
(setq inhibit-startup-message t)        ;no splash screen
;(setq ls-lisp-dirs-first t)             ;display dirs first in dired

;; Don't use dialog boxes for anything (since they seem to crash emacs on OS X...)
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
(set-face-attribute 'default nil :height 160 :font "Source Code Pro")
;; (if (string-equal (getenv "TERM") 'xterm-color)
;;     (message "Hello terminal!")
;;     (set-face-attribute 'default nil :height 180 :font "Source Code Pro")
;; )

;; Set the default frame size to be the full screen height and half the width
(add-to-list 'default-frame-alist (cons 'width 142))
(add-to-list 'default-frame-alist (cons 'height 42))
(setq initial-frame-alist '((top . 0) (left . 0)))

;; Set transparency default
;; (set-frame-parameter (selected-frame) 'alpha '(85 50))
;; (add-to-list 'default-frame-alist '(alpha 85 50))

;; Show line and column numbers
(line-number-mode 1)
(column-number-mode 1)

;; Hide menu, tool bar, and scroll bar
;;(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Better frame titles
(setq frame-title-format (concat  "%b - emacs@" system-name))

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
(setq-default indent-tabs-mode nil)
(standard-display-ascii ?\t "^I")

;; Set default "tab" width to 4 spaces
(setq tab-width 4)

;; Make "tab" give 2 spaces in c mode
(setq c-basic-indent 2)

;; Don't indent preprocessor directives
(c-set-offset (quote cpp-macro) 0 nil)

;; Set the number to the number of columns to use.
(setq-default fill-column 76)

;; Use mouse wheel
(mouse-wheel-mode t)

;; Make the bell visible, rather than audible
(setq visible-bell t)

;; Use command as the meta key
(setq mac-command-modifier 'meta)

;; Ignore the alt/option key
(setq mac-option-modifier nil)

;; Just follow git-controlled links without asking
(setq vc-follow-symlinks t)
