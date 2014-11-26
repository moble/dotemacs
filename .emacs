;; Start the server, if it's not already
(require 'server)
(or (server-running-p)
    (server-start))
(defvar server-buffer-clients)
(when (and (fboundp 'server-start) (string-equal (getenv "TERM") 'xterm))
  (server-start)
  (defun fp-kill-server-with-buffer-routine ()
    (and server-buffer-clients (server-done)))
  (add-hook 'kill-buffer-hook 'fp-kill-server-with-buffer-routine))

;; ;;Make sure to find my own lisp packages
;; (add-to-list 'load-path "/Users/boyle/.emacs.d/lisp")
;; (load "auctex.el" nil t t) ;; the melpa version doesn't work for me...

;; ;; Make sure the package manager is doing its thing
(require 'package)
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; Make sure auctex can find my stuff
(if (string-equal "darwin" (symbol-name system-type))
    (setenv "PATH" (concat "/usr/texbin:" (getenv "PATH"))))
(setq exec-path (append '("/usr/texbin") exec-path) )

;; Don't open new frame when emacs is called by the OS
(setq ns-pop-up-frames 'nil)

;; Override defaults
(fset 'yes-or-no-p 'y-or-n-p)           ;replace y-e-s by y
(setq inhibit-startup-message t)        ;no splash screen
;(setq ls-lisp-dirs-first t)             ;display dirs first in dired

;; Don't use dialog boxes for anything (since they seem to crash emacs on OS X...)
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice message-box (around prevent-dialog activate)
  "Prevent message-box from activating a dialog"
  (apply #'message (ad-get-args 0)))
(setq use-dialog-box nil)

;; Set font height to be taller and default to Adobe's Source Code Pro
(set-face-attribute 'default nil :height 160 :font "Source Code Pro")
;; (if (string-equal (getenv "TERM") 'xterm-color)
;;     (message "Hello terminal!")
;;     (set-face-attribute 'default nil :height 180 :font "Source Code Pro")
;; )

;; Set the default frame size to be the full screen height and half the width
(add-to-list 'default-frame-alist (cons 'width 142))
(add-to-list 'default-frame-alist (cons 'height 42))
(setq initial-frame-alist '((top . 0) (left . 0)))

;; Set transparency default
;; (set-frame-parameter (selected-frame) 'alpha '(85 50))
;; (add-to-list 'default-frame-alist '(alpha 85 50))

;; Show line and column numbers
(line-number-mode 1)
(column-number-mode 1)

;; Hide menu, tool bar, and scroll bar
;;(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Better frame titles
(setq frame-title-format (concat  "%b - emacs@" system-name))

;; When dragging with the mouse, automatically copy to the clipboard
(setq mouse-drag-copy-region t)

;; Complete with substrings (like ido)
(add-to-list 'completion-styles 'substring)
(setq read-file-name-completion-ignore-case t) ;; ignore case when completing file names
(setq read-buffer-completion-ignore-case t) ;; ignore case when completing buffer names

;; Make tramp use sshx by default
(require 'tramp)
(setq tramp-default-method "sshx")

;; When compiling, keep up with last line of output
(setq compilation-scroll-output t)

;; Helper for compilation. Close the compilation window
;; after exit if there was no error at all.
(defun compilation-exit-autoclose (status code msg)
  (when (and (eq status 'exit) (zerop code))
    (bury-buffer)
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  (cons msg code)
  )
(setq compilation-exit-message-function 'compilation-exit-autoclose)

;; Backup
(defvar backup-dir (expand-file-name (expand-file-name "~/.emacs.d/backup/")))
(defvar autosave-dir (expand-file-name (expand-file-name "~/.emacs.d/autosave/")))
(setq backup-directory-alist (list (cons ".*" backup-dir) (cons tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq backup-by-copying-when-linked t)

;; Retain indentation level when commenting
(setq comment-style 'indent)

;; Use spaces for all indents instead of tabs and show a tab character
;; as "^I".  [Note that `whitespace-cleanup` can do any necessary
;; conversions automatically.]
(setq-default indent-tabs-mode nil)
(standard-display-ascii ?\t "^I")

;; Set default "tab" width to 4 spaces
(setq tab-width 4)

;; Make "tab" give 2 spaces in c mode
(setq c-basic-indent 2)

;; Don't indent preprocessor directives
(c-set-offset (quote cpp-macro) 0 nil)

;; Set the number to the number of columns to use.
(setq-default fill-column 76)

;; Use mouse wheel
(mouse-wheel-mode t)

;; Make the bell visible, rather than audible
(setq visible-bell t)

;; Use command as the meta key
(setq mac-command-modifier 'meta)

;; Ignore the alt/option key
(setq mac-option-modifier nil)

;; Just follow git-controlled links without asking
(setq vc-follow-symlinks t)

;; To be able to use a manually installed solarized theme
;(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/color-themes/emacs-color-theme-solarized"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages from package manager ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Anaconda-mode is apparently integrated with anaconda itself
(add-hook 'python-mode-hook 'anaconda-mode)

;; yasnippet is used in a lot of auto-complete and company packages,
;; but has changed the names of some of its functions since those
;; packages were updated
(require 'yasnippet)
(defalias 'yas/current-snippet-table 'yas--get-snippet-tables)
(defalias 'yas/expand-snippet 'yas-expand-snippet)

;; ;; This makes sure we're using company in all modes
;; (add-hook 'after-init-hook 'global-company-mode)

;; ;; Clang autocompletion in C/C++ code, as installed by homebrew
;; ;; Requires auto-complete and flymake packages
;; ;;
;; ;; After homebrew install, I had to fix up the clang dylib with
;; ;;   chmod u+w /usr/local/opt/llvm/lib/libclang.dylib
;; ;;   install_name_tool -add_rpath "/usr/local/opt/llvm/lib/" /usr/local/opt/llvm/lib/libclang.dylib
;; ;;   chmod u-w /usr/local/opt/llvm/lib/libclang.dylib
;; (require 'auto-complete-clang-async)
;; ;; (defun ac-common-setup ()
;; ;;   ;; Just a dummy placeholder, in case I want something more
;; ;;   )
;; ;; (defun ac-cc-mode-setup ()
;; ;;   (setq ac-clang-complete-executable "~/.emacs.d/clang-complete")
;; ;;   (setq ac-sources '(ac-source-clang-async))
;; ;;   (ac-clang-launch-completion-process)
;; ;; )
;; ;; (defun my-ac-config ()
;; ;;   (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;; ;;   (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;; ;;   (global-auto-complete-mode t))
;; ;; (my-ac-config)
;; (setq ac-clang-complete-executable "~/.emacs.d/clang-complete")
;; (setq ac-sources '(ac-source-clang-async))
;; (ac-clang-launch-completion-process)
;; (global-auto-complete-mode t)

;; ;; From auto-complete-c-headers
;; (require 'auto-complete-c-headers)
;; ;; (add-to-list 'ac-sources 'ac-source-c-headers)
;; ;; From ac-c-headers
;; (add-hook 'c-mode-hook
;;           (lambda ()
;;             (add-to-list 'ac-sources 'ac-source-c-headers)
;;             (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))

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
;; Make emacs assume multi-file projects by default
                                        ;(setq-default TeX-master nil)

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
(add-to-list 'zotelo-translators (quote (Better-BibTeX "ca65189f-8815-4afe-8c8b-8c7c15f0edca" "bib")))
(setq zotelo-default-translator (quote Better-BibTeX))
;; The above two lines were translated from the following, which used to appear in custom-set-variables:
 ;; '(zotelo-default-translator (quote Better-BibTeX))
 ;; '(zotelo-translators (quote ((Better-BibTeX "ca65189f-8815-4afe-8c8b-8c7c15f0edca" "bib") (BibTeX "9cb70025-a888-4a29-a210-93ec52da40d4" "bib") (BibLaTeX "b6e39b57-8942-4d11-8259-342c46ce395f" "bib") (BibLaTeX-cite "fe7a85a9-4cb5-4986-9cc3-e6b47d6660f7" "bib") (Zotero-RDF "14763d24-8ba0-45df-8f52-b8d1108e7ac9" "rdf") (Wikipedia "3f50aaac-7acc-4350-acd0-59cb77faf620" "txt") (CSL-JSON "bc03b4fe-436d-4a1f-ba59-de4d2d7a63f7" "json") (Bookmarks-HTML "4e7119e0-02be-4848-86ef-79a64185aad8" "html") (Refer/BibIX "881f60f2-0802-411a-9228-ce5f47b64c7d" "txt") (RIS "32d59d2d-b65a-4da4-b0a3-bdd3cfb979e7" "ris") (MODS "0e2235e7-babf-413c-9acf-f27cce5f059c" "xml") (Bibliontology-RDF "14763d25-8ba0-45df-8f52-b8d1108e7ac9" "rdf")))))


;; Don't italicize \em and surrounds
(eval-after-load "font-latex"
  '(setq-default
    font-latex-match-italic-declaration-keywords
    (remove "em" font-latex-match-italic-declaration-keywords))
  )
;; (eval-after-load "font-latex"
;;   '(setq-default
;;     font-latex-match-italic-declaration
;;     "\\\\\\(\\(?:\\(?:it\\(?:shape\\)?\\|sl\\(?:shape\\)?\\)\\)\\>\\)")
;;   )


;; Markdown mode is nice for markdown files
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
;; (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; In case I need to look at matlab files (I'll never look at Objective/C files
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))



;;;;;;;;;;;;;;;;;;;;;;
;; Custom functions ;;
;;;;;;;;;;;;;;;;;;;;;;
(defun kill-other-buffer-and-window () (interactive)
  (other-window 1)
  (kill-buffer-and-window))
(defun set-frame-position-one () (interactive)
  (set-frame-position (selected-frame) 0 0)
  )
;; (defun set-frame-position-two () (interactive)
;;   (set-frame-position (selected-frame) (- 2880 (* 22 (frame-width))) 0)
;;   )
(defun set-frame-size-tex () (interactive)
  (set-frame-size (selected-frame) 70 42)
  )
(defun toggle-frame-size () (interactive)
  (if (< (frame-width (selected-frame)) 71)
      (set-frame-size (selected-frame) 85 42)
    (set-frame-size (selected-frame) 70 42)
    )
  )
(defun set-frame-size-full () (interactive)
  (set-frame-size (selected-frame) 142 42)
  )
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value)
  )
(defun night ()
  "Load the wombat theme, appropriate for nighttime work."
  (interactive)
                                        ;(load-theme 'solarized-dark t)
  (load-theme 'wombat t)
  )
(defun day ()
  "Disable the wombat theme, for bright environments, where wombat is hard to see."
  (interactive)
                                        ;(load-theme 'solarized-light t)
  (disable-theme 'wombat)
  )
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil))
  )
;;; http://stackoverflow.com/q/768243/1194883
(defun swap-words (a b)
  "Replace all occurances of a with b and vice versa"
  (interactive "*sFirst Swap Word: \nsSecond Swap Word: ")
  (save-excursion
    (while (re-search-forward (concat (regexp-quote a) "\\|" (regexp-quote b)))
      (if (equal (match-string 0) a)
          (replace-match (regexp-quote b))
        (replace-match (regexp-quote a)))
      )))
(defun swap-words-query (a b)
  "Replace all occurances of a with b and vice versa, prompting for each"
  (interactive "*sFirst Swap Word: \nsSecond Swap Word: ")
  (save-excursion
    (while (re-search-forward (concat (regexp-quote a) "\\|" (regexp-quote b)))
      (if (y-or-n-p "Swap?")
          (if (equal (match-string 0) a)
              (replace-match (regexp-quote b))
            (replace-match (regexp-quote a)))
        )
      )))
;; http://stackoverflow.com/a/24838407/1194883
(defun next-interesting-buffer ()
  (interactive)
  (let (( bread-crumb (buffer-name) ))
    (next-buffer)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (not ( equal bread-crumb (buffer-name) )) )
      (next-buffer))))
(defun previous-interesting-buffer ()
  (interactive)
  (let (( bread-crumb (buffer-name) ))
    (previous-buffer)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (not ( equal bread-crumb (buffer-name) )) )
      (previous-buffer))))

;;;;;;;;;;;;;;;;;;
;; Key bindings ;;
;;;;;;;;;;;;;;;;;;

(load "~/.dotemacs/keybindings")

;;;;;;;;;;;;;;;;;
;; Major modes ;;
;;;;;;;;;;;;;;;;;

;; Cython `pyx` files are basically python
;; (require 'cython-mode)
;; (setq auto-mode-alist
;;       (append '(("\\.pyx$" . cython-mode)
;;     ("\\.pxd$" . cython-mode)
;;     ) auto-mode-alist))

;; Ipython `.ipy` files should be treated as python
(setq auto-mode-alist
      (append '(("\\.ipy$" . python-mode)
                ) auto-mode-alist))

;; I sometimes use .tpp and .ipp extensions for c++ files
(setq auto-mode-alist
      (append '(("\\.tpp$" . c++-mode)
                ("\\.ipp$" . c++-mode)
                ) auto-mode-alist))

;; Asymptote mode
(add-to-list 'load-path "/usr/local/texlive/2013/texmf-dist/asymptote")
(autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
(autoload 'lasy-mode "asy-mode.el" "hybrid Asymptote/Latex major mode." t)
(autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
(add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))
(defun my-asy-mode-hook ()
  (setq compilation-window-height 8)
                                        ;(setq asy-compilation-buffer 'never)
  )
(add-hook 'asy-mode-hook 'my-asy-mode-hook)

;; Tikz/PGF modes
(add-to-list 'auto-mode-alist '("\\.tikz$" . latex-mode))


;;;;;;;;;;;;;;;;;
;; Minor modes ;;
;;;;;;;;;;;;;;;;;

;; redo
;;(require 'redo+)

;; auto-fill
(add-hook 'text-mode-hook 'auto-fill-mode)

;; flyspell
(require 'flyspell)
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-extra-args '("--sug-mode=fast"))

;; aspell, not ispell
(setq-default ispell-program-name "/usr/local/bin/aspell")

;; ;; icomplete+ (shows possible completions in the minibuffer)
;; (require 'icomplete+)
;; (icomplete-mode 1)

;; Define my own mode for encrypted files
(define-minor-mode sensitive-mode
  "For sensitive files like password lists.  It disables backup creation and auto saving.
   With no argument, this command toggles the mode.  Non-null prefix argument turns on the mode;
   null prefix argument turns off the mode."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Sensitive"
  ;; The minor mode bindings.
  nil
  (if (symbol-value sensitive-mode)
      (progn
        ;; disable backups
        (set (make-local-variable 'backup-inhibited) t)
        ;; disable auto-save
        (if auto-save-default
            (auto-save-mode -1))) ;resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited) ;resort to default auto save setting
    (if auto-save-default
        (auto-save-mode 1))))
(add-hook 'epa-file-find-file-hook #'sensitive-mode)

;; ;; screen-lines
;; (require 'screen-lines)
;; (add-hook 'text-mode-hook 'screen-lines-mode)
;; ;;(add-hook 'find-file-hooks 'screen-lines-mode)
;; (autoload 'screen-lines-mode "screen-lines"
;;   "Toggle Screen Lines minor mode for the current buffer." t)
;; (autoload 'turn-off-screen-lines-mode "screen-lines"
;;   "Turn off Screen Lines minor mode for the current buffer." t)
;; (autoload 'turn-on-screen-lines-mode "screen-lines"
;;   "Turn on Screen Lines minor mode for the current buffer." t)
;; (add-hook 'screen-lines-load-hook
;;     '(lambda ()
;;         (ad-disable-advice 'kill-line 'around 'screen-lines)
;;         (ad-activate 'kill-line)))


;; Auto compression mode
(auto-compression-mode 1)

;; Highlights selections
(setq transient-mark-mode t)

;; Use ipython --pylab when starting a python interpreter
;; (require 'ipython)
;; (setq ipython-command "ipython --pylab")
;; (setq py-python-command-args '("--pylab"))

;; CEDET
;; (semantic-mode 1)
;; (global-ede-mode t)
;; (setq semantic-default-submodes
;;       '(global-semanticdb-minor-mode
;;   global-semantic-idle-scheduler-mode
;;   global-semantic-idle-summary-mode
;;   global-semantic-idle-completions-mode
;;   global-semantic-decoration-mode
;;   global-semantic-highlight-func-mode
;;   global-semantic-stickyfunc-mode))
;; (ede-cpp-root-project "Triton" :file "/Users/boyle/Research/Triton/Makefile")



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Set automatically by emacs's customization ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-amsmath-label "Eq:")
 '(LaTeX-command "latex -file-line-error -synctex=1 --shell-escape")
 '(LaTeX-eqnarray-label "Eq:")
 '(LaTeX-equation-label "Eq:")
 '(LaTeX-figure-label "Fig:")
 '(LaTeX-indent-environment-list
   (quote
    (("verbatim" current-indentation)
     ("verbatim*" current-indentation)
     ("array")
     ("displaymath")
     ("eqnarray")
     ("eqnarray*")
     ("picture")
     ("tabbing")
     ("table")
     ("table*")
     ("tabular")
     ("tabular*"))))
 '(LaTeX-item-indent -1)
 '(LaTeX-label-function (quote reftex-label))
 '(LaTeX-section-label
   (quote
    (("part" . "Part:")
     ("chapter" . "Chapter:")
     ("section" . "Sec:")
     ("subsection" . "Sec:")
     ("subsubsection" . "Sec:"))))
 '(LaTeX-table-label "Table:")
 '(PDFLaTeX-amsmath-label "Eq:")
 '(PDFLaTeX-command
   "pdflatex -file-line-error -synctex=1 -shell-escape -interaction=nonstopmode ")
 '(PDFLaTeX-eqnarray-label "Eq:")
 '(PDFLaTeX-equation-label "Eq:")
 '(PDFLaTeX-figure-label "Fig:")
 '(PDFLaTeX-label-function (quote reftex-label))
 '(PDFLaTeX-section-label
   (quote
    (("part" . "Part:")
     ("chapter" . "Chapter:")
     ("section" . "Sec:")
     ("subsection" . "Sec:")
     ("subsubsection" . "Sec:"))))
 '(PDFLaTeX-table-label "Table:")
 '(TeX-PDF-mode t)
 '(TeX-command "tex -file-line-error --shell-escape")
 '(TeX-expand-list
   (quote
    (("%p" TeX-printer-query)
     ("%q"
      (lambda nil
        (TeX-printer-query t)))
     ("%V"
      (lambda nil
        (TeX-source-correlate-start-server-maybe)
        (TeX-view-command-raw)))
     ("%vv"
      (lambda nil
        (TeX-source-correlate-start-server-maybe)
        (TeX-output-style-check TeX-output-view-style)))
     ("%v"
      (lambda nil
        (TeX-source-correlate-start-server-maybe)
        (TeX-style-check TeX-view-style)))
     ("%r"
      (lambda nil
        (TeX-style-check TeX-print-style)))
     ("%l"
      (lambda nil
        (TeX-style-check LaTeX-command-style)))
     ("%(PDF)"
      (lambda nil
        (if
            (and
             (eq TeX-engine
                 (quote default))
             (or TeX-PDF-mode TeX-DVI-via-PDFTeX))
            "pdf" "")))
     ("%(PDFout)"
      (lambda nil
        (cond
         ((and
           (eq TeX-engine
               (quote xetex))
           (not TeX-PDF-mode))
          " -no-pdf")
         ((and
           (eq TeX-engine
               (quote luatex))
           (not TeX-PDF-mode))
          " --output-format=dvi")
         ((and
           (eq TeX-engine
               (quote default))
           (not TeX-PDF-mode)
           TeX-DVI-via-PDFTeX)
          " \"\\pdfoutput=0 \"")
         (t ""))))
     ("%(mode)"
      (lambda nil
        (if TeX-interactive-mode "" " -interaction=nonstopmode")))
     ("%(o?)"
      (lambda nil
        (if
            (eq TeX-engine
                (quote omega))
            "o" "")))
     ("%(tex)"
      (lambda nil
        (eval
         (nth 2
              (assq TeX-engine
                    (TeX-engine-alist))))))
     ("%(latex)"
      (lambda nil
        (eval
         (nth 3
              (assq TeX-engine
                    (TeX-engine-alist))))))
     ("%(execopts)" ConTeXt-expand-options)
     ("%S" TeX-source-correlate-expand-options)
     ("%dS" TeX-source-specials-view-expand-options)
     ("%cS" TeX-source-specials-view-expand-client)
     ("%(outpage)"
      (lambda nil
        (if TeX-source-correlate-output-page-function
            (funcall TeX-source-correlate-output-page-function)
          "1")))
     ("%s" file nil t)
     ("%t" file t t)
     ("%`"
      (lambda nil
        (setq TeX-command-pos t TeX-command-text "")))
     (" \"\\"
      (lambda nil
        (if
            (eq TeX-command-pos t)
            (setq TeX-command-pos pos pos
                  (+ 3 pos))
          (setq pos
                (1+ pos)))))
     ("\""
      (lambda nil
        (if
            (numberp TeX-command-pos)
            (setq TeX-command-text
                  (concat TeX-command-text
                          (substring command TeX-command-pos
                                     (1+ pos)))
                  command
                  (concat
                   (substring command 0 TeX-command-pos)
                   (substring command
                              (1+ pos)))
                  pos TeX-command-pos TeX-command-pos t)
          (setq pos
                (1+ pos)))))
     ("%'"
      (lambda nil
        (prog1
            (if
                (stringp TeX-command-text)
                (progn
                  (setq pos
                        (+
                         (length TeX-command-text)
                         9)
                        TeX-command-pos
                        (and
                         (string-match " "
                                       (funcall file t t))
                         "\""))
                  (concat TeX-command-text " "))
              (setq TeX-command-pos nil)
              "")
          (setq TeX-command-text nil))))
     ("%n" TeX-current-line)
     ("%d" file "dvi" t)
     ("%f" file "ps" t)
     ("%o"
      (lambda nil
        (funcall file
                 (TeX-output-extension)
                 t)))
     ("%b" TeX-current-file-name-master-relative)
     ("%m" preview-create-subdirectory))))
 '(TeX-output-view-style
   (quote
    (("^dvi$"
      ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$")
      "%(o?)dvips -t landscape %d -o && gv %f")
     ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f")
     ("^dvi$"
      ("^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "^landscape$")
      "%(o?)xdvi %dS -paper a4r -s 0 %d")
     ("^dvi$" "^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "%(o?)xdvi %dS -paper a4 %d")
     ("^dvi$"
      ("^a5\\(?:comb\\|paper\\)$" "^landscape$")
      "%(o?)xdvi %dS -paper a5r -s 0 %d")
     ("^dvi$" "^a5\\(?:comb\\|paper\\)$" "%(o?)xdvi %dS -paper a5 %d")
     ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d")
     ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d")
     ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d")
     ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d")
     ("^dvi$" "." "%(o?)xdvi %dS %d")
     ("^html?$" "." "open %o"))))
 '(TeX-save-query nil)
 '(TeX-shell "/bin/tcsh")
 '(TeX-shell-command-option "-c")
 '(TeX-view-program-list
   (quote
    (("Skim-displayline-locate" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b && osascript -e 'tell application \"Skim\" to set bounds of window 1 to {710, 0, 1450, 877}'")
     ("Skim-displayline" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b"))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Skim-displayline-locate")
     (output-html "xdg-open"))))
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(asy-command "asy ")
 '(confirm-kill-emacs (quote y-or-n-p))
 '(custom-safe-themes
   (quote
    ("7a9f392481b6e2fb027ab9d8053ab36c0f23bf5cc1271206982339370d894c74" "ed81411169b1b3e3d4cfc39b09d68ea13e0ff7708dc5b9d0bedb319e071968ad" "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" "11d069fbfb0510e2b32a5787e26b762898c7e480364cbc0779fe841662e4cf5d" default)))
 '(ede-project-directories (quote ("/Users/boyle/Research/Triton")))
 '(markdown-enable-math t)
 '(pdf-view-command "Preview")
 '(reftex-default-label-alist-entries
   (quote
    (AMSTeX amsmath endnotes fancybox floatfig longtable picinpar rotating sidecap subfigure supertab wrapfig LaTeX)))
 '(reftex-derive-label-parameters (quote (10 50 t nil "" nil nil)))
 '(reftex-insert-label-flags (quote ("sfte" "sfte")))
 '(safe-local-variable-values
   (quote
    ((zotero-collection .
                        #("127" 0 3
                          (name "Papers/Precession Dynamics")))
     (zotero-collection . "125")
     (zotero-collection .
                        #("83" 0 2
                          (name "Papers/Transformations of Scri+")))
     (zotero-collection . "61")
     (TeX-master . true)
     (LocalWords . namespace)
     (asy-TeX-master-file . "../../PresentationContent")
     (TeX-auto-generate-global . t)
     (TeX-master . t)
     (TeX-parse-self . t)
     (TeX-auto-save . t))))
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(show-trailing-whitespace t))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 190 :width normal :foundry "apple" :family "Monaco"))))
;;  )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
