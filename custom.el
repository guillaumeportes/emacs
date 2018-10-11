;;; package --- Summary
;;; Commentary:
;;; My first proper Emacs config
;;; Code:

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(defvar my-packages)
(setq my-packages '(all-the-icons
		 doom-themes
		 omnisharp
		 company
		 flycheck
		 ivy
		 counsel
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
		 git-gutter))

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

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

;(global-set-key "M-x" 'counsel-M-x)

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

;;; custom.el ends here
