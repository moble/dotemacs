(defun kill-other-buffer-and-window () (interactive)
  (other-window 1)
  (kill-buffer-and-window))

;; Resize and reshape the window
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
