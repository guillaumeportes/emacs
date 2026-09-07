;;; package --- common lisp stuff

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl --dynamic-space-size 8192")

(require 'slime-autoloads)
(setq slime-contribs '(slime-fancy))

;; (load "~/quicklisp/log4slime-setup.el")
;; (global-log4slime-mode 1)

(add-hook 'slime-repl-mode-hook #'completion-preview-mode)

(setq slime-net-coding-system 'utf-8-unix)
(setq slime-lisp-implementations
      '((sbcl ("sbcl" "--dynamic-space-size" "4096")
              :coding-system utf-8-unix)))
