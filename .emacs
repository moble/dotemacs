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

;; To be able to use a manually installed solarized theme
;(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/color-themes/emacs-color-theme-solarized"))


(load "~/.dotemacs/basic-behavior")

(load "~/.dotemacs/packages")

(load "~/.dotemacs/tex")

(load "~/.dotemacs/custom-functions")

(load "~/.dotemacs/keybindings")

(load "~/.dotemacs/major-modes")

(load "~/.dotemacs/minor-modes")



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
