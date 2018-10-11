;;; package --- Summary
;;; Commentary:
;;; My first proper Emacs config
;;; Code:

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

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

(defvar my-packages)
(setq my-packages '(all-the-icons
		 doom-themes
		 omnisharp
		 company
		 flycheck
		 ivy
		 counsel
		 swiper
		 auto-package-update
		 doom-modeline
		 projectile
		 counsel-projectile
		 perspective
		 persp-projectile
		 magit
		 which-key
		 ivy-rich
		 exec-path-from-shell
		 ggtags
		 git-gutter
		 org-plus-contrib
		 ace-window
		 smex))

(dolist (p my-packages)
  (straight-use-package p))

(require 'doom-themes)
(setq doom-themes-enable-bold t)
(setq doom-themes-enable-italic t)
(load-theme 'doom-one t)
(doom-themes-visual-bell-config)

(require 'company)
(eval-after-load
 'company
 '(add-to-list 'company-backends 'company-omnisharp))

(add-hook 'csharp-mode-hook #'company-mode)
(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-to-list 'company-backends 'company-omnisharp)

(require 'ivy)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(ivy-mode 1)

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

(require 'auto-package-update)
(setq auto-package-update-delete-old-versions t)
(auto-package-update-maybe)

(doom-modeline-init)

(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-enable-caching t)
(setq projectile-switch-project-action 'projectile-dired)

(counsel-projectile-mode)

(persp-mode)

(define-key projectile-mode-map (kbd "s-s") 'projectile-persp-switch-project)

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

(require 'git-gutter)
(global-git-gutter-mode +1)
(setq inhibit-splash-screen t)
(transient-mark-mode 1)
(setq ns-right-alternate-modifier (quote none))

(require 'org)
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(require 'ace-window)
(global-set-key (kbd "M-p") 'ace-window)

(smex-initialize)
(global-set-key (kbd "C-x g") 'magit-status)

;;; custom.el ends here
