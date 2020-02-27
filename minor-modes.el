;; Better redo; See keybindings to "M-z" and "M-S-z"
(require 'redo+)

;; auto-fill when typing long lines in text files
(add-hook 'text-mode-hook 'auto-fill-mode)

;; aspell, not ispell
(setq-default ispell-program-name "/usr/local/bin/aspell")

;; flyspell
(require 'flyspell)
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-extra-args '("--sug-mode=fast"))

;; company-mode completion
(add-hook 'after-init-hook 'global-company-mode)

;; global activation of the unicode symbol completion for company-mode
(add-to-list 'company-backends 'company-math-symbols-unicode)

;; Auto compression mode
(auto-compression-mode 1)

;; Highlights selections
(setq transient-mark-mode t)

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
