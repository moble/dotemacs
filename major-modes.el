;; Ipython `.ipy` files should be treated as python
(setq auto-mode-alist
      (append '(("\\.ipy$" . python-mode)) auto-mode-alist))

;; I sometimes use .tpp and .ipp extensions for c++ files
(setq auto-mode-alist
      (append '(("\\.tpp$" . c++-mode) ("\\.ipp$" . c++-mode)) auto-mode-alist))

;; Asymptote mode
(add-to-list 'load-path "/usr/local/texlive/2013/texmf-dist/asymptote")
(autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
(autoload 'lasy-mode "asy-mode.el" "hybrid Asymptote/Latex major mode." t)
(autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
(add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))
(defun my-asy-mode-hook () (setq compilation-window-height 8) )
(add-hook 'asy-mode-hook 'my-asy-mode-hook)

;; Tikz/PGF modes
(add-to-list 'auto-mode-alist '("\\.tikz$" . latex-mode))

;; Markdown mode is nice for markdown files
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
;; (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-enable-math t)
;; (require 'mmm-mode)
;; (setq mmm-global-mode 'maybe)
;; (mmm-add-classes
;;  '((markdown-tex
;;     :submode tex-mode
;;     :front "^ *\\begin"
;;     :back "^ *\\end"
;;     :include-front t
;;     :include-back t
;;     :end-not-begin t)))
;; (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-tex)

;; In case I need to look at matlab files (I'll never look at Objective/C
;; files, which is the default for a .m file)
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
