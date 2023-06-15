;; -*- lexical-binding: t -*-

;; straight.el stuff

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)

;; general settings

(setq history-length 25)
(savehist-mode 1)
(save-place-mode 1)
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
(setq use-dialog-box nil)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq mac-option-modifier 'meta)
(toggle-truncate-lines -1)
(setq truncate-partial-width-windows nil)
(electric-pair-mode 1)
(show-paren-mode 1)
(recentf-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)

;; keys

(global-set-key (kbd "C-,") (kbd "<backspace>"))
(global-set-key (kbd "C-<") 'backward-kill-word)
(global-set-key (kbd "C-S-d") 'kill-word)
(global-set-key (kbd "C-S-b") 'backward-word)
(global-set-key (kbd "C-S-f") 'forward-word)
(global-set-key (kbd "C-<tab>") 'tab-next)
(global-set-key (kbd "C-S-<tab>") 'tab-previous)
(define-key isearch-mode-map (kbd "C-,") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)


;; ui

(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

(toggle-frame-maximized)
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(setq visible-bell t)
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                shell-mode-hook
                vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "Fira Mono" :height 140)
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 140)
(set-face-attribute 'variable-pitch nil :font "Hack" :height 160 :weight 'regular)

(straight-use-package 'all-the-icons)
(require 'all-the-icons)
(unless (find-font (font-spec :name "all-the-icons"))
  (all-the-icons-install-fonts t))

(straight-use-package 'all-the-icons-completion)
(all-the-icons-completion-mode 1)

(mapc #'disable-theme custom-enabled-themes)

(straight-use-package 'doom-modeline)
(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-height 15)

(straight-use-package 'doom-themes)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
(load-theme 'doom-one t)
(doom-themes-visual-bell-config)
(doom-themes-org-config)

;; (straight-use-package 'ef-themes)
;; (require 'ef-themes)
;; (load-theme 'ef-light :no-confirm)

(straight-use-package 'avy)
(setq avy-keys (number-sequence ?a ?z))
(require 'avy)
(global-set-key (kbd "C-'") 'avy-goto-word-1-below)
(global-set-key (kbd "C-\"") 'avy-goto-word-1-above)

(straight-use-package 'rainbow-delimiters)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(straight-use-package 'highlight-parentheses)
(require 'highlight-parentheses)
(add-hook 'prog-mode-hook 'highlight-parentheses-mode)

;; helpful

(straight-use-package 'helpful)
(require 'helpful)

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)

;; Completion

(straight-use-package 'vertico)
(require 'vertico)
(vertico-mode 1)
(require 'vertico-directory "extensions/vertico-directory.el")
(define-key vertico-map (kbd "RET") 'vertico-directory-enter)
(define-key vertico-map (kbd "DEL") 'vertico-directory-delete-char)
(define-key vertico-map (kbd "M-DEL") 'vertico-directory-delete-word)
(add-hook 'vertico-mode-hook #'rfn-eshadow-update-overlay)
(add-hook 'vertico-mode-hook #'vertico-directory-tidy)

(setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(setq read-extended-command-predicate #'command-completion-default-include-p)
(setq enable-recursive-minibuffers t)

(straight-use-package 'orderless)
(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(straight-use-package 'marginalia)
(require 'marginalia)
(define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)
(marginalia-mode 1)

(straight-use-package 'consult)
(require 'consult)
(consult-customize consult-buffer :preview-key "C-x b")
(consult-customize consult-project-buffer :preview-key "C-x p b")
(consult-customize consult-mark :preview-key "M-g m")
(consult-customize consult-imenu :preview-key "M-g i")

(consult-customize
 consult--source-buffer
 consult--source-bookmark consult--source-recent-file
 consult--source-project-recent-file :preview-key (kbd "M-."))

(straight-use-package 'pulsar)
(require 'pulsar)
(setq pulsar-pulse-functions
      '(isearch-repeat-forward
        isearch-repeat-backward
        recenter-top-bottom
        move-to-window-line-top-bottom
        reposition-window
        bookmark-jump
        other-window
        delete-window
        delete-other-windows
        forward-page
        backward-page
        scroll-up-command
        scroll-down-command
        windmove-right
        windmove-left
        windmove-up
        windmove-down
        windmove-swap-states-right
        windmove-swap-states-left
        windmove-swap-states-up
        windmove-swap-states-down
        tab-new
        tab-close
        tab-next))

(setq pulsar-pulse t)
(setq pulsar-delay 0.055)
(setq pulsar-iterations 10)
(setq pulsar-face 'pulsar-magenta)
(setq pulsar-highlight-face 'pulsar-yellow)

(pulsar-global-mode 1)

;; Window management

(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C->") (lambda () (interactive) (other-window -1)))

(tab-bar-mode 1)
(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil)

;(setq split-height-threshold nil)
;(setq split-width-threshold 0)

(winner-mode 1)

(straight-use-package 'hydra)

(defvar hydra-resize-window-amount 5)

(defhydra hydra-resize-window ()
  "Resize window."
  ("i" (enlarge-window hydra-resize-window-amount) "enlarge vertically")
  ("k" (shrink-window hydra-resize-window-amount) "shrink vertically")
  ("l" (enlarge-window-horizontally hydra-resize-window-amount) "enlarge horizontally")
  ("j" (shrink-window-horizontally hydra-resize-window-amount) "shrink horizontally"))

(defhydra hydra-windmove ()
  "Move window."
  ("i" (windmove-up) "move up")
  ("k" (windmove-down) "move down")
  ("j" (windmove-left) "move left")
  ("l" (windmove-right) "move right"))

(define-prefix-command 'window-key-map)
(define-key 'window-key-map (kbd "m") 'hydra-windmove/body)
(define-key 'window-key-map (kbd "s")
  (lambda ()
    (interactive)
    (if (equal nil current-prefix-arg)
        (setf hydra-resize-window-amount 5)
      (setf hydra-resize-window-amount current-prefix-arg))
    (hydra-resize-window/body)))

(global-set-key (kbd "C-c w") 'window-key-map)

(defhydra hydra-winner ()
  "Winner undo / redo."
  ("j" (progn (winner-undo) (setq this-command 'winner-undo)) "winner undo")
  ("l" (winner-redo) "winner redo"))
(define-key 'window-key-map (kbd "w") 'hydra-winner/body)

(defhydra hydra-other-window (:timeout 4)
  "Other window."
  ("n" (other-window 1) "next")
  ("p" (other-window -1) "previous")
  ("f" nil "finished" :exit t))

;(define-key (current-global-map) [remap other-window] (lambda ()
;							(interactive)
;							(other-window 1)
;							(hydra-other-window/body)))

;; dired

; n - next line
; p - previous line
; RET / C-m - open file
; ^ - parent directory
; v - preview mode (q to close)
; o - open in other window
; C-o - open in other window without focussing
; dired-jump - open dired in the directory of the current buffer file
; j - dired-goto-file
; f - dired-find-file
; ( - dired-hide-details

(setq dired-listing-switches "-agho --group-directories-first")
(setq insert-directory-program "/usr/local/bin/gls")
(setq delete-by-moving-to-trash t)
(global-set-key (kbd "C-x C-g") 'dired-jump)

(straight-use-package 'dired-single)
(straight-use-package 'all-the-icons-dired)
(if (display-graphic-p)
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(straight-use-package 'command-log-mode)
(require 'command-log-mode)

(straight-use-package 'diminish)
(require 'diminish)

(straight-use-package 'which-key)
(require 'which-key)
(which-key-mode 1)
(diminish 'which-key-mode)
(setq which-key-idle-delay 0.3)

;; vc

(straight-use-package 'git-gutter)
(require 'git-gutter)
(global-git-gutter-mode 1)

(straight-use-package 'magit)
(require 'magit)
(define-key magit-section-mode-map (kbd "C-<tab>") nil)
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

;; shell

(straight-use-package 'exec-path-from-shell)
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(straight-use-package 'vterm)
(require 'vterm)
(setq vterm-max-scrollback 10000)

(straight-use-package 'multi-vterm)
(require 'multi-vterm)

(defun configure-eshell ()
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  ;(define-key eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (setq eshell-history-size 10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(add-hook 'eshell-first-time-mode-hook 'configure-eshell)
(with-eval-after-load 'esh-opt
  (setq eshell-destroy-buffer-when-process-dies t)
  (setq eshell-visual-commands '("htop" "zsh" "vim"))
  (eshell-git-prompt-use-theme 'powerline))

(straight-use-package 'eshell-git-prompt)
(require 'eshell-git-prompt)

;; languages

;; (straight-use-package 'flycheck)
;; (require 'flycheck)
;; (global-flycheck-mode 1)

(straight-use-package 'flymake)
(require 'flymake)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
(add-hook 'emacs-lisp-mode-hook 'flymake-mode)

(straight-use-package 'solidity-mode)
(require 'solidity-mode)
(setq solidity-comment-style 'slash)
(define-key solidity-mode-map (kbd "C-c C-g") 'solidity-estimate-gas-at-point)

;; (straight-use-package 'company-solidity)
;; (require 'company-solidity)

;;; eglot

(straight-use-package 'project)
(straight-use-package 'xref)
(straight-use-package 'eldoc)
(straight-use-package 'jsonrpc)
(straight-use-package 'eglot)
(require 'eglot)
(add-hook 'csharp-mode-hook 'eglot-ensure)
(add-hook 'solidity-mode-hook 'eglot-ensure)
(add-hook 'sh-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs
             `(solidity-mode . ("solc" "--lsp")))
(add-to-list 'eglot-server-programs
             `(csharp-mode . ("~/omnisharp-osx-arm64-net6.0/OmniSharp" "-lsp")))


;;; corfu

(straight-use-package '(corfu :files ("*" (:exclude ".git") "extensions/*")))
(require 'corfu)
(global-corfu-mode)
(corfu-popupinfo-mode)
(setq corfu-popupinfo-delay 0.1)
(setq corfu-auto t)
(define-key corfu-map (kbd "C-m") nil)

(straight-use-package 'kind-icon)
(require 'kind-icon)
(setq kind-icon-default-face 'corfu-default)
(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

;;; cape

(straight-use-package 'cape)
(require 'cape)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-file)

;;; company

;; (straight-use-package 'company)
;; (require 'company)
;; (setq company-global-modes '(not vterm-mode))
;; (global-company-mode)
;; (define-key company-mode-map (kbd "<tab>") 'company-complete-selection)
;; (define-key company-active-map (kbd "C-m") nil)
;; (define-key company-active-map (kbd "C-SPC") #'company-complete-selection)

;; (setq company-minimum-prefix-length 1)
;; (setq company-idle-delay 0.0)
;; (setq company-show-quick-access t)
;; (setq company-backends '(company-elisp company-capf))

;; (straight-use-package 'company-box)
;; (require 'company-box)
;; (add-hook 'company-mode-hook 'company-box-mode)

;; (straight-use-package 'company-lsp)
;; (require 'company-lsp)

(straight-use-package 'evil-nerd-commenter)
(require 'evil-nerd-commenter)
(global-set-key (kbd "C-c C-c") 'evilnc-comment-or-uncomment-lines)

;;; common lisp
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;;; slime

;; (straight-use-package '(slime-cape :type git :host github :repo "ccqpein/slime-cape"))
;; (straight-use-package 'slime)
;; (require 'slime)

;; (require 'slime-autoloads)
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (setq common-lisp-style-default "sbcl")
;; (slime-setup '(slime-fancy slime-repl slime-scratch slime-trace-dialog slime-cl-indent slime-cape))

;;; sly
(straight-use-package 'sly)
(require 'sly)

(defun enable-sly-tramp ()
  (interactive)
  (add-to-list 'sly-filename-translations (sly-create-filename-translator :machine-instance (sly-machine-instance)
                                                                          :remote-host "ccg-server"
                                                                          :username "root")))

(defun disable-sly-tramp ()
  (interactive)
  (setq sly-filename-translations (remove (nth (1- (length sly-filename-translations)) sly-filename-translations) sly-filename-translations)))

(add-to-list 'tramp-default-method-alist '("" "root" "ssh"))
(customize-set-variable 'tramp-default-method "ssh")

(load "~/quicklisp/log4sly-setup.el")
(global-log4sly-mode 1)

;; (global-set-key (kbd "C-c s") #'slime-selector)

(global-set-key (kbd "C-M-S-j") 'backward-sexp)
(global-set-key (kbd "C-M-:") 'forward-sexp)
(global-set-key (kbd "C-M-S-k") #'(lambda () (interactive) (re-search-backward "[()]")))
(global-set-key (kbd "C-M-S-l") #'(lambda () (interactive) (re-search-forward "[()]")))

;;; csharp
(straight-use-package 'csharp-mode)
(require 'csharp-mode)
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

;;; eww

(setq
 browse-url-browser-function 'eww-browse-url ; Use eww as the default browser
 shr-use-fonts  nil                          ; No special fonts
 shr-use-colors nil                          ; No colours
 shr-indentation 2                           ; Left-side margin
 shr-width 70                                ; Fold text to 70 columns
 eww-search-prefix "https://wiby.me/?q=")    ; Use another engine for searching

(straight-use-package 'docker)
(require 'docker)
(straight-use-package 'docker-compose-mode)
(require 'docker-compose-mode)
(straight-use-package 'dockerfile-mode)
(require 'dockerfile-mode)

;;; latex

(straight-use-package 'auctex)

;;; medium

;;; org

;;; (define-key org-mode-map (kbd "C-,") 'org-delete-backward-char)

(provide 'init)
;;; init.el ends here
