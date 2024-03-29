;; This command can be used to run another command on all files in a directory.
;; Open the directory as usual (which will be in `dired`), and mark all the
;; desired files by pressing `m`.  Then, press `M-x dired-do-command ENTER
;; <command-name> ENTER`.  Save and close all affected buffers, as desired.
;; Credit: <http://superuser.com/a/176629/213106>
(defun dired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved; others will just remain open.  Files are selected in dired with `m`"
  (interactive "CRun on marked files M-x ")
  (save-window-excursion
    (mapc (lambda (filename)
            (find-file filename)
            (call-interactively command))
          (dired-get-marked-files))))

(defun kill-other-buffer-and-window () (interactive)
  (other-window 1)
  (kill-buffer-and-window))

;; Resize and reshape the window
(defun set-frame-position-one () (interactive)
  (set-frame-position (selected-frame) 0 0)
  )

;; Previously: 70x50 and faces 130, 110
;; Previously: 70x52 and faces 150, 130
(defun set-frame-size-tex () (interactive)
  (set-face-attribute 'default nil :height 150)
  (set-face-attribute 'mode-line nil :height 130)
  (set-frame-size (selected-frame) 70 58)
  (set-frame-position-one)
)

;; Previously: 142x42 and faces 160, 140
;; Previously: 151x43 and faces 180, 160
;; Previously: 151x43 and faces 180, 160 for Source Code Pro
;; Previously: 151x43 and faces 180, 160 for Fira Code
(defun set-frame-size-full () (interactive)
  (set-face-attribute 'default nil :height 190)
  (set-face-attribute 'mode-line nil :height 160)
  (set-frame-size (selected-frame) 151 48)
  (set-frame-position-one)
)

(defun toggle-frame-size () (interactive)
  (if
    (< (frame-width (selected-frame)) 71)
    (set-frame-size-full)
    (set-frame-size-tex)
  )
)

;; Make the window transparent
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value)
  )

;; To switch between my bright and dark themes
(defun night ()
  "Load the wombat theme, appropriate for nighttime work."
  (interactive)
  (load-theme 'wombat t)
  )
(defun day ()
  "Disable the wombat theme, for bright environments, where wombat is hard to see."
  (interactive)
  (disable-theme 'wombat)
  )

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil))
  )

;; Swap strings of characters (a bit finicky, but better than nothing)
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

;; When going to the next/previous, skip buffers starting with "*"
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


;; Jump to matching delimiters <http://www.emacswiki.org/emacs/NavigatingParentheses>
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis. Else go to the
   opening parenthesis one level up."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1))
        (t
         (backward-char 1)
         (cond ((looking-at "\\s\)")
                (forward-char 1) (backward-list 1))
               (t
                (while (not (looking-at "\\s("))
                  (backward-char 1)
                  (cond ((looking-at "\\s\)")
                         (message "->> )")
                         (forward-char 1)
                         (backward-list 1)
                         (backward-char 1)))
                  ))))))

(defun tolerate-tabs ()
  "Switch to displaying tabs as tabs"
  (interactive)
  (standard-display-ascii ?\t "\t")
  )


;; ;; The following doesn't work
;; (defun search-in-math ()
;;   "Call `query-replace-regexp' with `isearch-filter-predicate' set to filter out matches outside LaTeX math environments."
;;   (interactive)
;;   (let ((isearch-filter-predicate
;;          (lambda (BEG END)
;;            (save-excursion (save-match-data (goto-char BEG) (texmathp)))))
;;         (case-fold-search nil))
;;     (call-interactively 'isearch-forward)))


(defun replace-in-math ()
  "Call `query-replace-regexp' with `isearch-filter-predicate' set to filter out matches outside LaTeX math environments."
  (interactive)
  (let ((isearch-filter-predicate
         (lambda (BEG END)
           (save-excursion (save-match-data (goto-char BEG) (texmathp)))))
        (case-fold-search nil))
    (call-interactively 'query-replace-regexp)))
