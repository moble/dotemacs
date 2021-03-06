;; Added by Package.el.  Just leave it commented out; I'll do it again in packages.el.
;;(package-initialize)

;; Start the server, if it's not already
(require 'server)
(unless (server-running-p)
    (server-start))
(defvar server-buffer-clients)
(when (and (fboundp 'server-start) (string-equal (getenv "TERM") 'xterm))
  (server-start)
  (defun fp-kill-server-with-buffer-routine ()
    (and server-buffer-clients (server-done)))
  (add-hook 'kill-buffer-hook 'fp-kill-server-with-buffer-routine))


;; Load all my goodies from other files
(load "~/.dotemacs/packages")
(load "~/.dotemacs/basic-behavior")
(load "~/.dotemacs/tex")
(load "~/.dotemacs/custom-functions")
(load "~/.dotemacs/keybindings")
(load "~/.dotemacs/major-modes")
(load "~/.dotemacs/minor-modes")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set automatically by emacs's customization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dad-joke typescript-mode web-mode org-bullets latex-extra zotelo redo+ python-docstring markdown-mode auctex)))
 '(safe-local-variable-values
   (quote
    ((TeX-master . "../Presentation")
     (TeX-master . "paper")
     (asy-TeX-master-file . "../Presentation")
     (asy-TeX-master-file . "Presentation")
     (TeX-master . true)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
