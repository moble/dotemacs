;; These two are usually bound to minimization, which I hate passionately
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(put 'suspend-frame 'disabled t)

(global-set-key (kbd "M-e") 'execute-extended-command)
(global-set-key (kbd "M-l") 'goto-line)
(global-set-key (kbd "C-c C-c") 'compile)

;; Make some keys more Mac-like
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "M-a") 'mark-whole-buffer)
(global-set-key (kbd "M-f") 'isearch-forward-regexp)
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-o") 'find-file)
;; Cut/copy/paste
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-v") 'yank)
;;(global-set-key (kbd "M-x") 'kill-region) ;; Maybe don't go overboard
;; Undo/redo properly
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "M-S-z") 'redo)
;; "Zoom" the text
(define-key global-map (kbd "M-=") 'text-scale-increase)
(define-key global-map (kbd "M--") 'text-scale-decrease)
(define-key global-map (kbd "M-0") 'text-scale-mode)

;; This behavior makes emacs buffers behave like browser and terminal tabs, and
;; skips useless buffers
(global-set-key (kbd "M-S-<right>") 'next-interesting-buffer)
(global-set-key (kbd "M-S-<left>") 'previous-interesting-buffer)

;; These correspond to some of my custom functions
(global-set-key (kbd "M-1") 'set-frame-position-one)
(global-set-key (kbd "M-t") 'toggle-frame-size)
(global-set-key (kbd "M-n") 'night)
(global-set-key (kbd "M-d") 'day)
(global-set-key (kbd "M-\\") 'goto-match-paren)
