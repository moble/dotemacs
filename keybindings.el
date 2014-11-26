;; These two are usually bound to minimization, which I hate passionately
(global-unset-key "\C-z")
(global-unset-key "\C-x\C-z")
(put 'suspend-frame 'disabled t)

;; Make some keys more Mac-like
(global-set-key "\M-`" 'other-frame)
(global-set-key [(meta a)] 'mark-whole-buffer)
(global-set-key "\M-f" 'isearch-forward)
;; Cut/copy/paste
(global-set-key "\M-c" 'kill-ring-save)
(global-set-key "\M-v" 'yank)
(global-set-key "\M-x" 'kill-region)
;; Undo/redo properly
(global-set-key [(meta z)] 'undo)
(global-set-key [C-z] 'undo)
(global-set-key [(meta shift z)] 'redo)
;; "Zoom" the text
(define-key global-map (kbd "M-=") 'text-scale-increase)
(define-key global-map (kbd "M--") 'text-scale-decrease)
(define-key global-map (kbd "M-0") 'text-scale-mode)

;; This behavior makes emacs buffers behave like browser and terminal tabs, and
;; skips useless buffers
(global-set-key (kbd "M-S-<right>") 'next-interesting-buffer)
(global-set-key (kbd "M-S-<left>") 'previous-interesting-buffer)

;; These correspond to some of my custom functions
(global-set-key "\M-1" 'set-frame-position-one)
(global-set-key "\M-t" 'toggle-frame-size)
(global-set-key [(meta n)] 'night)
(global-set-key [(meta d)] 'day)

(global-set-key [(meta l)] 'goto-line)
(global-set-key "\C-c\C-c" 'compile)
