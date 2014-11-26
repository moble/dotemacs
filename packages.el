;; Anaconda-mode is apparently integrated with anaconda itself
(add-hook 'python-mode-hook 'anaconda-mode)

;; yasnippet is used in a lot of auto-complete and company packages,
;; but has changed the names of some of its functions since those
;; packages were last updated
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




