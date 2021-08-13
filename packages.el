;; This must come before configurations of installed packages
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
)

;; Use my anaconda installation for python
(defvar python-shell-interpreter)
(setq python-shell-interpreter
      (expand-file-name "~/.continuum/current/envs/science/bin/python"))

;; ;; Tell everyone about my conda installation
;; (require 'conda)
;; (conda-env-initialize-interactive-shells)
;; (conda-env-initialize-eshell)
;; (conda-env-autoactivate-mode nil)
;; (setq conda-anaconda-home (file-truename "/Users/boyle/.continuum/current"))
;; (setq conda-env-home-directory (file-truename "/Users/boyle/.continuum/current"))
;; (conda-env-activate "science")

;; ;; Anaconda-mode is apparently integrated with anaconda itself
;; (add-hook 'python-mode-hook 'anaconda-mode)

;; ;; Document function parameters in echo area
;; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; This makes sure we're using company in all modes
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Zotelo is dealt with in tex.el

;; (add-to-list 'load-path "path-to-julia-mode")
;; (require 'julia-mode)
