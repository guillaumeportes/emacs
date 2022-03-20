;; -*- lexical-binding: t -*-

;; straight.el stuff

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
(savehist-mode 1)
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

(straight-use-package 'doom-modeline)
(require 'doom-modeline)
(doom-modeline-mode 1)
;(setq doom-modeline-height 15)

(straight-use-package 'doom-themes)
(require 'doom-themes)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-one t)

(straight-use-package 'beacon)
(require 'beacon)
(beacon-mode 1)

(straight-use-package 'dimmer)
(require 'dimmer)
(dimmer-configure-which-key)
(dimmer-configure-company-box)
(dimmer-configure-hydra)
(dimmer-configure-magit)
(dimmer-mode 1)

(straight-use-package 'avy)
(setq avy-keys (number-sequence ?a ?z))
(require 'avy)
(global-set-key (kbd "C-'") 'avy-goto-word-1-below)
(global-set-key (kbd "C-\"") 'avy-goto-word-1-above)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)
;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)
;; or for treemacs users
(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
(doom-themes-treemacs-config)
;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

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
(add-hook 'rfn-eshadow-update-overlay-hook 'vertico-directory-tidy)

(straight-use-package 'orderless)
(require 'orderless)
(setq completion-styles '(basic partial-completion orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(straight-use-package 'marginalia)
(require 'marginalia)
(define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)
(marginalia-mode 1)

(straight-use-package 'consult)
(require 'consult)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-y") 'consult-yank-from-kill-ring)
(global-set-key (kbd "M-y") 'consult-yank-pop)
(global-set-key (kbd "M-g g") 'consult-goto-line)
(global-set-key (kbd "M-g M-g") 'consult-goto-line)
(global-set-key (kbd "M-g m") 'consult-mark)
(global-set-key (kbd "M-g i") 'consult-imenu)
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "C-S-s") 'consult-line-multi)

(straight-use-package 'embark)
(require 'embark)
(global-set-key (kbd "C-:") 'embark-act)
(global-set-key (kbd "C-;") 'embark-export)
(global-set-key (kbd "C-h B") 'embark-bindings)

(straight-use-package 'embark-consult)
(require 'embark-consult)
(add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode)

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
  "resize window"
  ("i" (enlarge-window hydra-resize-window-amount) "enlarge vertically")
  ("k" (shrink-window hydra-resize-window-amount) "shrink vertically")
  ("l" (enlarge-window-horizontally hydra-resize-window-amount) "enlarge horizontally")
  ("j" (shrink-window-horizontally hydra-resize-window-amount) "shrink horizontally"))

(defhydra hydra-windmove ()
  "move window"
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
  "winner undo / redo"
  ("j" (progn (winner-undo) (setq this-command 'winner-undo)) "winner undo")
  ("l" (winner-redo) "winner redo"))
(define-key 'window-key-map (kbd "w") 'hydra-winner/body)

(defhydra hydra-other-window (:timeout 4)
  "other window"
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
(git-gutter-mode +1)

(straight-use-package 'magit)
(require 'magit)
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

;;; flycheck

(straight-use-package 'flycheck)
(require 'flycheck)
(global-flycheck-mode 1)

;;; lsp

(straight-use-package 'lsp-mode)
(require 'lsp-mode)
(add-hook 'csharp-mode-hook 'lsp-deferred)

;;; corfu

;; (straight-use-package 'corfu)
;; (require 'corfu)
;; (setq corfu-auto 1)
;; (corfu-global-mode 1)
;; (global-set-key (kbd "M-/") 'dabbrev-completion)
;; (add-hook 'csharp-mode-hook 'corfu-mode)

;; (straight-use-package 'kind-icon)
;; (require 'kind-icon)
;; (setq kind-icon-default-face 'corfu-default)
;; (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

;; (straight-use-package '(corfu-doc :type git :host github :repo "galeo/corfu-doc"))
;; (require 'corfu-doc)
;; (add-hook 'corfu-mode-hook #'corfu-doc-mode)
;; (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)
;; (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down)

;; (setq tab-always-indent 'complete)

;;; company

(straight-use-package 'company)
(require 'company)
(global-company-mode)
(define-key company-mode-map (kbd "<tab>") 'company-complete-selection)
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0.0)
(setq company-show-quick-access t)

(straight-use-package 'company-box)
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)

(straight-use-package 'company-lsp)
(require 'company-lsp)

(straight-use-package 'evil-nerd-commenter)
(require 'evil-nerd-commenter)
(global-set-key (kbd "C-c C-c") 'evilnc-comment-or-uncomment-lines)

;;; common lisp

(straight-use-package 'slime)
(straight-use-package 'slime-company)
(require 'slime-autoloads)
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(slime-setup '(slime-fancy slime-company))

(straight-use-package 'lispy)
(require 'lispy)
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'lisp-mode-hook (lambda () (lispy-mode 1)))
(define-key lispy-map-keymap (kbd "C-,") nil)
(define-key lispy-mode-map (kbd "C-,") nil)
(define-key lispy-mode-map-lispy (kbd "C-,") nil)
(define-key lispy-mode-map (kbd "<DEL>") nil)
(define-key lispy-mode-map-lispy (kbd "<DEL>") nil)
(define-key lispy-mode-map (kbd ")") nil)
(define-key lispy-mode-map-lispy (kbd ")") nil)
(define-key lispy-mode-map (kbd "(") nil)
(define-key lispy-mode-map-lispy (kbd "(") nil)
(define-key lispy-mode-map (kbd "M-w") nil)
(define-key lispy-mode-map-lispy (kbd "M-w") nil)
(define-key lispy-mode-map (kbd "C-y") nil)
(define-key lispy-mode-map-lispy (kbd "C-y") nil)
(define-key lispy-mode-map (kbd "C-d") nil)
(define-key lispy-mode-map-lispy (kbd "C-d") nil)

;; (straight-use-package 'sly)
;; (require 'sly)

;;; csharp
(straight-use-package 'csharp-mode)
(require 'csharp-mode)
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

;;; solidity

(straight-use-package 'solidity-mode)
(require 'solidity-mode)
(setq solidity-comment-style 'slash)
(define-key solidity-mode-map (kbd "C-c C-g") 'solidity-estimate-gas-at-point)

(straight-use-package 'solidity-flycheck)
(require 'solidity-flycheck)
(setq solidity-flycheck-solc-checker-active t)
(setq solidity-flycheck-solium-checker-active t)
(setq flycheck-solidity-solc-addstd-contracts t)

(straight-use-package 'company-solidity)
(require 'company-solidity)

;; org

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

(defun org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

(defun org-mode-font-setup()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Hack" :weight 'regular :height (cdr face)))
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(add-hook 'org-mode-hook 'org-mode-setup)
(define-key org-mode-map "C-," nil)
(setq org-ellipsis " ▾"
      org-hide-emphasis-markers nil)
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)
(setq org-agenda-files
      '("~/tasks.org"
        "~/birthdays.org"
        "~/habits.org"))
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
                          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

(setq org-refile-targets
      '(("archive.org" :maxlevel . 1)
	("tasks.org" :maxlevel . 1)))

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-tag-alist
      '((:startgroup)
                                        ; Put mutually exclusive tags here
        (:endgroup)
        ("@errand" . ?E)
        ("@home" . ?H)
        ("@work" . ?W)
        ("agenda" . ?a)
        ("planning" . ?p)
        ("publish" . ?P)
        ("batch" . ?b)
        ("note" . ?n)
        ("idea" . ?i)))

;; Configure custom agenda views
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 7)))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))
          (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))
	
        ("n" "Next Tasks"
         ((todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))))

        ("W" "Work Tasks" tags-todo "+work-email")

        ;; Low-effort next actions
        ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
         ((org-agenda-overriding-header "Low Effort Tasks")
          (org-agenda-max-todos 20)
          (org-agenda-files org-agenda-files)))

        ("w" "Workflow Status"
         ((todo "WAIT"
                ((org-agenda-overriding-header "Waiting on External")
                 (org-agenda-files org-agenda-files)))
          (todo "REVIEW"
                ((org-agenda-overriding-header "In Review")
                 (org-agenda-files org-agenda-files)))
          (todo "PLAN"
                ((org-agenda-overriding-header "In Planning")
                 (org-agenda-todo-list-sublevels nil)
                 (org-agenda-files org-agenda-files)))
          (todo "BACKLOG"
                ((org-agenda-overriding-header "Project Backlog")
                 (org-agenda-todo-list-sublevels nil)
                 (org-agenda-files org-agenda-files)))
          (todo "READY"
                ((org-agenda-overriding-header "Ready for Work")
                 (org-agenda-files org-agenda-files)))
          (todo "ACTIVE"
                ((org-agenda-overriding-header "Active Projects")
                 (org-agenda-files org-agenda-files)))
          (todo "COMPLETED"
                ((org-agenda-overriding-header "Completed Projects")
                 (org-agenda-files org-agenda-files)))
          (todo "CANC"
                ((org-agenda-overriding-header "Cancelled Projects")
                 (org-agenda-files org-agenda-files)))))))

(setq org-capture-templates
      `(("t" "Tasks / Projects")
	("tt" "Task" entry (file+olp "~/tasks.org" "Inbox")
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

	("j" "Journal Entries")
	("jj" "Journal" entry
         (file+olp+datetree "~/journal.org")
         "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)
	("jm" "Meeting" entry
         (file+olp+datetree "~/journal.org")
         "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)

	("w" "Workflows")
	("we" "Checking Email" entry (file+olp+datetree "~/journal.org")
         "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

	("m" "Metrics Capture")
	("mw" "Weight" table-line (file+headline "~/metrics.org" "Weight")
	 "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

(define-key global-map (kbd "C-c j")
  (lambda () (interactive) (org-capture nil "jj")))

(org-mode-font-setup)

(straight-use-package 'org-bullets)
(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

(defun org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(straight-use-package 'visual-fill-column)
(require 'visual-fill-column)
(add-hook 'org-mode-hook 'org-mode-visual-fill)
