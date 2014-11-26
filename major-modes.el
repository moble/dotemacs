;; Ipython `.ipy` files should be treated as python
(setq auto-mode-alist
      (append '(("\\.ipy$" . python-mode)) auto-mode-alist))

;; I sometimes use .tpp and .ipp extensions for c++ files
(setq auto-mode-alist
      (append '(("\\.tpp$" . c++-mode) ("\\.ipp$" . c++-mode)) auto-mode-alist))

;; Tikz/PGF modes
(add-to-list 'auto-mode-alist '("\\.tikz$" . latex-mode))

;; Markdown mode is nice for markdown files
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
;; (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-enable-math t)

;; In case I need to look at matlab files (I'll never look at Objective/C
;; files, which is the default for a .m file)
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
