;; Make sure the package manager is doing its thing
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  ;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  ;;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
)


;; Anaconda-mode is apparently integrated with anaconda itself
(add-hook 'python-mode-hook 'anaconda-mode)

;; yasnippet is used in a lot of auto-complete and company packages,
;; but has changed the names of some of its functions since those
;; packages were last updated
(require 'yasnippet)
(defalias 'yas/current-snippet-table 'yas--get-snippet-tables)
(defalias 'yas/expand-snippet 'yas-expand-snippet)

;; This makes sure we're using company in all modes
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
