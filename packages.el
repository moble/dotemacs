;; This must come before configurations of installed packages
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
)

;; Use my anaconda installation for python
(defvar python-shell-interpreter)
(setq python-shell-interpreter
      (expand-file-name "~/.continuum/anaconda/bin/python"))

;; ;; Anaconda-mode is apparently integrated with anaconda itself
;; (add-hook 'python-mode-hook 'anaconda-mode)

;; ;; This makes sure we're using company in all modes
;; (require 'company)
;; (add-hook 'after-init-hook 'global-company-mode)

;; Zotelo is dealt with in tex.el
