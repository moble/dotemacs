;; Make sure auctex can find my latex executables.  Unfortunately, this seems
;; to be the correct way to give emacs a PATH.
(if (string-equal "darwin" (symbol-name system-type))
  (setenv "PATH" (concat "/usr/texbin:" (getenv "PATH"))))
(setq exec-path (append '("/usr/texbin") exec-path) )

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; ;; Standard things I generally don't need or like
;; (setq-default TeX-master nil) ;; Don't automatically set the master file name
;; (add-hook 'LaTeX-mode-hook 'visual-line-mode) ;;
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode) ;; Turn on flyspell for every tex file
;; (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode) ;; "Easier" entry of math symbols

(setq TeX-parse-self t) ;; Enable parsing of the file itself on load
(setq TeX-auto-save t) ;; Enable save on command executation (e.g., LaTeX)
(setq TeX-save-query nil) ;; Don't even ask about it

(setq TeX-electric-sub-and-superscript t) ;; auto-insert braces
(setq TeX-display-help nil) ;; Just show a terse minibuffer comment when inspecting errors
(add-hook 'LaTeX-mode-hook 'set-frame-size-tex) ;; resize to my dimensions
(setq reftex-label-alist '((nil ?e nil "~\\eqref{%s}" nil nil))) ;; Look for options for eqref

;; The following is used to set variables that cannot be set globally, but must
;; be set each time a tex file is opened
(defvar TeX-command-extra-options)
(add-hook 'TeX-mode-hook
  (lambda ()
    (setq TeX-command-extra-options "-shell-escape")
    (setq fill-column 70)
    (set-frame-size-tex)
    (TeX-add-symbols '("eqref" TeX-arg-ref (ignore)))
  )
)

;; Use Skim nicely
(setq TeX-source-correlate-mode t) ;; Use synctex
(defvar TeX-view-program-list)
(setq TeX-view-program-list '())
(add-to-list 'TeX-view-program-list
  '("displayline-locate"
    (concatenate
     ;; This uses the Skim app to jump straight to a line (%n) in the PDF file
     ;; (%o) corresponding to the tex file (%b).  The flag `-r` reverts the
     ;; file from disk if it was already open; `-g` keeps Skim in the
     ;; background (so I can keep editing); and `-b` shows the reading bar,
     ;; highlighting the PDF line corresponding to the point in the tex buffer.
     "/Applications/Skim.app/Contents/SharedSupport/displayline -r -b %n %o %b "
     "&& osascript -e 'tell application \"Skim\" to set bounds of window 1 to {650, 0, 1680, 1050}'")))
(defvar TeX-view-program-selection)
(setq TeX-view-program-selection '())
(add-to-list 'TeX-view-program-selection '(output-pdf "displayline-locate"))

;; Add useful arXiv fields to the Misc entry type (which an arxiv-only paper
;; needs to be, since bibtex complains about a missing journal if it's an
;; Article).
(defvar bibtex-BibTeX-entry-alist)
(add-hook
 'bibtex-mode-hook
 (lambda ()
   (add-to-list 'bibtex-BibTeX-entry-alist
    '("Misc" "Miscellaneous" nil nil
      (("author")
       ("title" "Title of the work (BibTeX converts it to lowercase)")
       ("month") ("year") ("note")
       ("eprint" "ArXiv identifier number: gr-qc/9601234 or 0707.1234")
       ("archivePrefix" "Probably just 'arXiv', for new-style arXiv IDs" "arXiv")
       ("primaryClass" "Class gr-qc or something like that" "gr-qc")
       ("SLACcitation" "Something hideous like: %%CITATION = HEP-TH/9108028;%%")
       ("howpublished" "The way in which the work was published, if none of the above"))
      )
    )
  )
)

;; Use zotelo to interface with my zotero library
;;
;; This requires the MozRepl plugin.  To install the plugin with Zotero
;; Standalone, google MozRepl and download (possibly by right-clicking and
;; choosing "Save as...").  You should get an .xpi file.  Open Zotero, click on
;; the "Tools" menu, then "Add-ons".  At the top-right of the window that pops
;; up, there's a little gear icon, under which you'll find "Install Add-on From
;; File...".  Click that, select your .xpi file, and follow the instructions.
;; Now, next time you open the "Tools" menu, there should be a MozRepl entry.
;; I like to enable the "Activate on startup" option.
;;
(add-hook 'TeX-mode-hook 'zotelo-minor-mode)
;; I figured out exactly what these lines should be by customizing through the
;; menu system, and just looking in custom-set-variables after saving
(eval-after-load "zotelo-minor-mode"
  '(progn
    (add-to-list 'zotelo-translators (quote (Better-BibTeX "ca65189f-8815-4afe-8c8b-8c7c15f0edca" "bib")))
    (setq zotelo-default-translator (quote Better-BibTeX))
    )
  )

;; Don't italicize \em and surrounds
(defvar font-latex-match-italic-declaration-keywords)
(eval-after-load "font-latex"
  '(setq-default
    font-latex-match-italic-declaration-keywords
    (remove "em" font-latex-match-italic-declaration-keywords))
  )

;; Allow certain variables to be set per-buffer within a file using either the
;; `Local Variables:` list (usually at the bottom of the file), or the `-*-`
;; string (usually near the top of the file).
(put 'TeX-auto-generate-global 'safe-local-variable #'booleanp)
(put 'TeX-master 'safe-local-variable #'booleanp)
(put 'TeX-parse-self 'safe-local-variable #'booleanp)
(put 'TeX-auto-save 'safe-local-variable #'booleanp)
;; See also basic-behavior.el
