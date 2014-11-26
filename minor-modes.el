
;; redo
(require 'redo+)

;; auto-fill
(add-hook 'text-mode-hook 'auto-fill-mode)

;; flyspell
(require 'flyspell)
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-extra-args '("--sug-mode=fast"))

;; aspell, not ispell
(setq-default ispell-program-name "/usr/local/bin/aspell")

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


;; Auto compression mode
(auto-compression-mode 1)

;; Highlights selections
(setq transient-mark-mode t)

;; Use ipython --pylab when starting a python interpreter
;; (require 'ipython)
;; (setq ipython-command "ipython --pylab")
;; (setq py-python-command-args '("--pylab"))
