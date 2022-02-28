;;; package --- Summary
;;; Commentary:
;;; My first proper Emacs config
;;; Code:

(setenv "FrameworkPathOverride" "/Library/Frameworks/Mono.framework/Versions/Current")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'subr-x)
(straight-use-package 'git)

(defvar my-packages)
(setq my-packages '(all-the-icons
		    doom-themes
		    omnisharp
		    company
		    company-quickhelp
		    company-flx
		    flycheck
		    ivy
		    counsel
		    swiper
		    doom-modeline
		    projectile
		    counsel-projectile
		    perspective
		    persp-projectile
		    magit
		    which-key
		    ivy-rich
		    exec-path-from-shell
		    git-gutter
		    ace-window
		    smex
		    yaml-mode
		    helpful
		    avy
		    easy-kill
		    highlight-parentheses
		    slime
		    slime-company
		    isearch-prop
		    isearch+
		    org-bullets
		    shader-mode
		    ))

(dolist (p my-packages)
  (straight-use-package p))

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

(show-paren-mode t)

(require 'doom-themes)
(setq doom-themes-enable-bold t)
(setq doom-themes-enable-italic t)
(load-theme 'doom-one t)
(doom-themes-visual-bell-config)

(require 'company)
(eval-after-load
    'company
  '(add-to-list 'company-backends 'company-omnisharp))

(defun my-csharp-mode-setup ()
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)
  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq evil-shift-width 4)

  ;csharp-mode README.md recommends this too
  ;(electric-pair-mode 1)       ;; Emacs 24
  ;(electric-pair-local-mode 1) ;; Emacs 25
  (electric-indent-mode)

  ;(local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
					;(local-set-key (kbd "C-c C-c") 'recompile))
  )

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

(require 'ivy)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))
(ivy-mode 1)

;(Global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2n> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

(doom-modeline-init)

(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
(setq projectile-enable-caching t)

(counsel-projectile-mode)

(persp-mode)

(define-key projectile-mode-map (kbd "C-c C-p p") 'projectile-persp-switch-project)

(which-key-mode)

(ivy-rich-mode)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(require 'exec-path-from-shell)
(require 'shell)
(setq explicit-shell-file-name "/usr/local/bin/bash")
(setq projectile-completion-system 'ivy)

(tool-bar-mode 0)
(scroll-bar-mode 0)
(global-flycheck-mode)
(add-hook  'after-init-hook 'global-company-mode)
(global-hl-line-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(toggle-frame-maximized)
(recentf-mode 1)
(global-display-line-numbers-mode t)
(setq display-line-numbers "%4d \u2502 ")

(require 'git-gutter)
(global-git-gutter-mode +1)
(setq inhibit-splash-screen t)
(transient-mark-mode 1)

;(setq mac-command-modifier 'meta)
;(setq mac-option-modifier 'control)

(require 'org)
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(require 'ace-window)
(global-set-key (kbd "C-.") 'ace-window)
(setq aw-scope 'frame)

(smex-initialize)
(global-set-key (kbd "C-x g") 'magit-status)

(setq projectile-switch-project-action 'projectile-dired)

(setq insert-directory-program (executable-find "gls"))

(require 'company-quickhelp)
(company-quickhelp-mode)

 (with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") (lambda () (interactive) (company-complete-common-or-cycle 1)))
  (define-key company-active-map (kbd "C-p") (lambda () (interactive) (company-complete-common-or-cycle -1))))

(setq company-idle-delay 0)
(setq company-show-numbers t)

(global-set-key (kbd "C-'") 'avy-goto-char-in-line)
(global-set-key (kbd "M-p") 'avy-pop-mark)
(global-set-key (kbd "C-;") 'avy-goto-char-2)

(electric-pair-mode 1)
(global-set-key [remap kill-ring-save] 'easy-kill)

(global-set-key (kbd "C-,") 'delete-backward-char)

(setq inferior-lisp-program "/usr/local/bin/sbcl")
(slime-setup '(slime-fancy slime-company))

(setq truncate-lines t)

(setq projectile-project-search-path '("~/dev"))

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-diff-options "-w")

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(defvar yank-indent-modes '(prog-mode
                            sgml-mode
                            js2-mode)
  "Modes in which to indent regions that are yanked (or yank-popped)")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (--any? (derived-mode-p it) yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defun yank-unindented ()
  (interactive)
  (yank 1))

(global-set-key (kbd "M-z") 'zap-up-to-char)

(defun backward-zap-up-to-char (c)
  "Backward zap up to C."
  (interactive "cBackward zap up to char:")
  (zap-up-to-char -1 c))

(global-set-key (kbd "M-Z") 'backward-zap-up-to-char)
;;; Custom.el ends here

(set-face-font 'default "Hack")
