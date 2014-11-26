;; Make sure auctex can find my stuff
(if (string-equal "darwin" (symbol-name system-type))
    (setenv "PATH" (concat "/usr/texbin:" (getenv "PATH"))))
(setq exec-path (append '("/usr/texbin") exec-path) )

;; ;; From ac-math
;; (require 'ac-math)
;; (add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`
;; (defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
;;   (setq ac-sources
;;         (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
;;                 ac-sources))
;;   )
;; (add-hook 'latex-mode-hook 'ac-latex-mode-setup)
;; (ac-flyspell-workaround)

;; ;; ;; Start us off with the manually installed auctex
;; (load "auctex.el" nil t t)

;; ;; From auto-complete-auctex
;; (require 'auto-complete-auctex)

;;;; AUCTeX
(require 'tex-site)
(setq TeX-parse-self t) ; Enable parse on load.
(setq TeX-auto-save t) ; Enable parse on save.
(setq TeX-electric-sub-and-superscript t) ; auto-insert braces
(setq TeX-display-help nil) ;; Just show a terse minibuffer comment when inspecting errors
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ; with AUCTeX LaTeX mode
(add-hook 'LaTeX-mode-hook 'set-frame-size-tex)
(setq reftex-plug-into-AUCTeX t)
(setq LaTeX-command-default "run")
(setq TeX-command-default "run")
;; The following only works with AUCTeX loaded
                                        ;(load "auctex.el" nil t t)
                                        ;(require 'tex-site)
(add-hook 'TeX-mode-hook
          (lambda ()
            (add-to-list 'TeX-output-view-style
                         '("^pdf$" "."
                           "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b && osascript -e 'tell application \"Skim\" to set bounds of window 1 to {710, 0, 1450, 877}'"))
            (add-to-list 'TeX-command-list
                         '("run" "latexmk %s" TeX-run-command t t :help "Run latexmk") t)
            )
          )

;; Use latex-extra for interesting keys like:
;;   * C-c C-a : latex/compile-commands-until-done
;;   * C-c C-n : latex/next-section
;;   * C-c C-p : latex/previous-section
;; (eval-after-load 'latex '(latex/setup-keybinds))

;; (require 'auctex-latexmk)
;; (auctex-latexmk-setup)

;;;; BIBTeX
(add-hook
 'bibtex-mode-hook
 (lambda ()
   (add-to-list
    'bibtex-BibTeX-entry-alist
    '("Misc" "Miscellaneous" nil nil
      (("author")
       ("title" "Title of the work (BibTeX converts it to lowercase)")
       ("eprint" "ArXiv identifier number")
       ("archivePrefix" "Probably just 'arXiv'" "arXiv")
       ("primaryClass" "Class gr-qc or something like that" "gr-qc")
       ("SLACcitation" "Something like: %%CITATION = HEP-TH/9108028;%%")
       ("howpublished" "The way in which the work was published")
       ("month") ("year") ("note"))
      )
    )
   )
 )

;; Use zotelo to interface with my zotero library
(add-hook 'TeX-mode-hook 'zotelo-minor-mode)
;; I figured out exactly what these lines should be by adjusting a default,
;; and just looking in custom-set-variables
(add-to-list 'zotelo-translators (quote (Better-BibTeX "ca65189f-8815-4afe-8c8b-8c7c15f0edca" "bib")))
(setq zotelo-default-translator (quote Better-BibTeX))


;; Don't italicize \em and surrounds
(eval-after-load "font-latex"
  '(setq-default
    font-latex-match-italic-declaration-keywords
    (remove "em" font-latex-match-italic-declaration-keywords))
  )
