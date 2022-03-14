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

(setq history-length 25)
(savehist-mode 1)
(save-place-mode 1)
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
(setq use-dialog-box nil)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

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

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 140)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Hack" :height 160 :weight 'regular)

(straight-use-package 'helpful)
(require 'helpful)

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)

(savehist-mode 1)

;; Completion
(straight-use-package 'vertico)
(vertico-mode 1)

(straight-use-package 'orderless)
(require 'orderless)
(setq completion-styles '(orderless))

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
(global-set-key (kbd "C-h B") 'embark-bindings)

(straight-use-package 'embark-consult)
(require 'embark-consult)
(add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode)

;; (straight-use-package 'ivy)
;; (require 'ivy)
;; (bind-key "C-s" 'swiper)
;; (bind-key "TAB" 'ivy-alt-done ivy-minibuffer-map)
;; (bind-key "C-l" 'ivy-alt-done ivy-minibuffer-map)
;; (bind-key "C-j" 'ivy-next-line ivy-minibuffer-map)
;; (bind-key "C-k" 'ivy-previous-line ivy-minibuffer-map)
;; (bind-key "C-k" 'ivy-previous-line ivy-switch-buffer-map)
;; (bind-key "C-l" 'ivy-done ivy-switch-buffer-map)
;; (bind-key "C-d" 'ivy-switch-buffer-kill ivy-switch-buffer-map)
;; (bind-key "C-k" 'ivy-previous-line ivy-reverse-i-search-map)
;; (bind-key "C-d" 'ivy-reverse-i-search-kill ivy-reverse-i-search-map)
;; (setq ivy-use-virtual-buffers t)
;; (setq ivy-count-format "(%d/%d) ")
;; (setq ivy-re-builders-alist
;;       '((t . ivy--regex-plus)))
;; (ivy-mode 1)

;; (straight-use-package 'all-the-icons-ivy-rich)
;; (require 'all-the-icons-ivy-rich)
;; (all-the-icons-ivy-rich-mode 1)

;; (straight-use-package 'ivy-rich)
;; (require 'ivy-rich)
;; (ivy-rich-mode 1)

;; (straight-use-package 'counsel)
;; (require 'counsel)
;; (bind-key (kbd "M-x") 'counsel-M-x)
;; (bind-key (kbd "C-x b") 'counsel-switch-buffer)
;; (bind-key (kbd "C-x C-f") 'counsel-find-file)
;; (bind-key (kbd "C-r") 'counsel-minibuffer-history 'minibuffer-local-map)
;; (unbind-key "C-," counsel-describe-map)
;; (unbind-key "C-." counsel-describe-map)

;; (setq counsel-describe-function-function #'helpful-callable)
;; (setq counsel-describe-variable-function #'helpful-variable)
;; (define-key global-map [remap describe-function] 'counsel-describe-function)
;; (define-key help-map [remap describe-function] 'counsel-describe-function)
;; (define-key global-map [remap describe-command] 'helpful-command)
;; (define-key global-map [remap describe-variable] 'counsel-describe-variable)
;; (define-key help-map [remap describe-variable] 'counsel-describe-variable)
;; (define-key global-map [remap describe-key] 'helpful-key)
;; (define-key help-map [remap describe-key] 'helpful-key)

;; Project management

;; (straight-use-package 'projectile)
;; (require 'projectile)
;; (diminish 'projectile-mode)
;; (projectile-mode 1)
;; (setq projectile-completion-system 'ivy)
;; (bind-key (kbd "C-c C-p") 'projectile-command-map)
;; (when (file-directory-p "~/dev")
;;   (setq projectile-project-search-path '("~/dev")))
;; (setq projectile-switch-project-action #'projectile-dired)
;; (setq projectile-enable-caching t)
;; (straight-use-package 'counsel-projectile)
;; (require 'counsel-projectile)
;; (counsel-projectile-mode 1)

;; Window management

;; (straight-use-package 'perspective)
;; (require 'perspective)
;; (bind-key (kbd "C-x C-b") 'persp-list-buffers)
;; (add-hook 'kill-emacs-hook 'persp-state-save)
;; (persp-mode 1)
;; (setq persp-state-default-file "~/emacs/.perspective")

;; (straight-use-package 'persp-projectile)
;; (require 'persp-projectile)
;; (define-key projectile-mode-map (kbd "C-c C-p p") 'projectile-persp-switch-project)

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

(straight-use-package 'slime)
(require 'slime-autoloads)
(setq inferior-list-program "usr/local/bin/sbcl")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

;; Automatically tangle our emacs.org config file when we save it
(defun org-babel-tangle-config()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/emacs/emacs.org"))
    ;; Dynamic scoping to the rescure
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'org-babel-tangle-config)))

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

(straight-use-package 'all-the-icons)
(require 'all-the-icons)
(unless (find-font (font-spec :name "all-the-icons"))
  (all-the-icons-install-fonts t))

(straight-use-package 'doom-modeline)
(require 'doom-modeline)
(doom-modeline-mode 1)
;(setq doom-modeline-height 15)

(straight-use-package 'doom-themes)
(require 'doom-themes)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-one t)

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

(straight-use-package 'diminish)
(require 'diminish)

(straight-use-package 'which-key)
(require 'which-key)
(which-key-mode 1)
(diminish 'which-key-mode)
(setq which-key-idle-delay 0.3)

;(setq split-width-threshold 1)

(setq mac-option-modifier 'meta)

(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C->") (lambda () (interactive) (other-window -1)))

(global-set-key (kbd "C-,") (kbd "<backspace>"))

(defhydra hydra-other-window (:timeout 4)
  "other window"
  ("n" (other-window 1) "next")
  ("p" (other-window -1) "previous")
  ("f" nil "finished" :exit t))

;(define-key (current-global-map) [remap other-window] (lambda ()
;							(interactive)
;							(other-window 1)
;							(hydra-other-window/body)))

(straight-use-package 'git-gutter)
(require 'git-gutter)
(git-gutter-mode +1)

(straight-use-package 'magit)
(require 'magit)
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

(straight-use-package 'lsp-mode)
(require 'lsp-mode)
(setq lsp-keymap-prefix "C-c l")
(lsp-enable-which-key-integration t)
(setq lsp-signature-render-documentation nil)
(setq lsp-disabled-clients '(csharp-ls))

(recentf-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

(straight-use-package 'csharp-mode)
(require 'csharp-mode)
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))
(setq tab-width 4)
(setq c-basic-offset 4)
(setq c-syntactic-indentation t)

(add-hook 'csharp-mode-hook 'lsp-deferred)
(add-hook 'js-mode-hook 'lsp-deferred)

(setq truncate-lines t)

(straight-use-package 'smex)
(require 'smex)
(smex-initialize)

(straight-use-package 'exec-path-from-shell)
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(straight-use-package 'company)
(require 'company)
(add-hook 'lsp-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'company-mode)
(define-key company-mode-map (kbd "<tab>") 'company-complete-selection)
(define-key lsp-mode-map (kbd "<tab>") 'company-indent-or-complete-common)
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0.0)
(setq company-show-quick-access t)

(straight-use-package 'company-box)
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)

(straight-use-package 'lsp-ui)
(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(setq lsp-ui-doc-position 'bottom)
(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-sideline-show-hover nil)

;(straight-use-package 'lsp-ivy)
;(require 'lsp-ivy)

(straight-use-package 'evil-nerd-commenter)
(require 'evil-nerd-commenter)
(global-set-key (kbd "M-/") 'evilnc-comment-or-uncomment-lines)

(straight-use-package 'flycheck)
(require 'flycheck)
(global-flycheck-mode 1)

(toggle-frame-maximized)

(straight-use-package 'highlight-parentheses)
(require 'highlight-parentheses)
(add-hook 'prog-mode-hook 'highlight-parentheses-mode)

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
  (setq eshell-visual-commands '("htop" "zsh" "vim")))
;(eshell-git-prompt-use-theme 'powerline)

(straight-use-package 'eshell-git-prompt)
(require 'eshell-git-prompt)

(toggle-truncate-lines -1)
(setq truncate-partial-width-windows nil)

(straight-use-package 'python-mode)
(require 'python-mode)
(add-hook 'python-mode-hook 'lsp-deferred)
(setq python-shell-interpreter "ipython")
(setq python-shell-interpreter-args "--colors=Linux --profile=default --simple-prompt --pprint")
(setq python-shell-prompt-regexp "In \\[[0-9]+\\]: ")
(setq python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
(setq python-shell-completion-setup-code "from IPython.core.completerlib import module_completion")
(setq python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n")
(setq python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
(setq python-shell-completion-native-enable nil)

(straight-use-package 'pyvenv)
(require 'pyvenv)
(pyvenv-mode 1)

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
