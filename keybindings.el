;; "Zoom" the text
(define-key global-map (kbd "M-=") 'text-scale-increase)
(define-key global-map (kbd "M--") 'text-scale-decrease)
(define-key global-map (kbd "M-0") 'text-scale-mode)

(global-set-key [(meta a)] 'mark-whole-buffer)
(global-set-key "\M-`" 'other-frame)

(global-set-key [(meta z)] 'undo)
(global-set-key [C-z] 'undo)
(global-set-key [(meta shift z)] 'redo)

;; 'delete-frame just happens too often by accident
(global-set-key "\M-w" 'kill-ring-save) ;;'delete-frame)

(global-set-key "\M-1" 'set-frame-position-one)
;; (global-set-key "\M-2" 'set-frame-position-two)
(global-set-key "\M-t" 'toggle-frame-size)
(global-set-key "\M-f" 'set-frame-size-full)

(global-set-key [(meta l)] 'goto-line)
;; (global-set-key [f5] 'kill-other-buffer-and-window)
;; (global-set-key [f8] 'ediff-buffers)
;; (global-set-key [f9] 'ediff-revision)

(global-set-key "\M-c" 'compile)
(global-set-key [kp-delete] 'delete-char)
(global-set-key [C-backspace] 'backward-kill-word)
(global-set-key [C-kp-delete] 'kill-word)
(global-set-key [home] 'move-beginning-of-line)
(global-set-key [end] 'move-end-of-line)
(global-set-key [(meta n)] 'night)
(global-set-key [(meta d)] 'day)
(global-set-key (kbd "M-S-<right>") 'next-interesting-buffer)
(global-set-key (kbd "M-S-<left>") 'previous-interesting-buffer)

;; These two are usually bound to minimization, which I hate passionately
(global-unset-key "\C-z")
(global-unset-key "\C-x\C-z")
(put 'suspend-frame 'disabled t)
