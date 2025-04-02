;; packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(defvar packages
  '(all-the-icons all-the-icons-completion doom-modeline ef-themes avy rainbow-delimiters highlight-parentheses helpful vertico orderless marginalia consult pulsar hydra all-the-icons-dired command-log-mode diminish which-key git-gutter magit exec-path-from-shell vterm multi-vterm eshell-git-prompt eldoc jsonrpc corfu nerd-icons-corfu evil-nerd-commenter sly auctex ztree markdown-mode copilot copilot-chat embark embark-consult vundo)
  "Packages installed at launch.")

(dolist (p packages)
  (unless (package-installed-p p)
    (package-install p)))

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

(global-set-key (kbd "C-;") 'dabbrev-expand)
(glo)

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
(menu-bar-mode 1)
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

(require 'all-the-icons)
(unless (find-font (font-spec :name "all-the-icons"))
  (all-the-icons-install-fonts t))

(all-the-icons-completion-mode 1)

(mapc #'disable-theme custom-enabled-themes)

(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-height 15)

;; (setq doom-themes-enable-bold t
;;       doom-themes-enable-italic t)
;; (load-theme 'doom-one t)
;; (doom-themes-visual-bell-config)
;; (doom-themes-org-config)

;; (require 'modus-themes)
;; (load-theme 'modus-vivendi :no-confirm)

(require 'ef-themes)
(load-theme 'ef-summer :no-confirm)

(setq avy-keys (number-sequence ?a ?z))
(require 'avy)
(global-set-key (kbd "C-'") 'avy-goto-word-1-below)
(global-set-key (kbd "C-\"") 'avy-goto-word-1-above)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'highlight-parentheses)
(add-hook 'prog-mode-hook 'highlight-parentheses-mode)

;; helpful

(require 'helpful)

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)

;; Completion

(require 'vertico)
(vertico-mode 1)
(require 'vertico-directory)
(define-key vertico-map (kbd "RET") 'vertico-directory-enter)
(define-key vertico-map (kbd "DEL") 'vertico-directory-delete-char)
(define-key vertico-map (kbd "M-DEL") 'vertico-directory-delete-word)
(add-hook 'vertico-mode-hook #'rfn-eshadow-update-overlay)
(add-hook 'vertico-mode-hook #'vertico-directory-tidy)

(setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(setq read-extended-command-predicate #'command-completion-default-include-p)
(setq enable-recursive-minibuffers t)

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))
                                      (eglot (styles orderless))))

(require 'marginalia)
(define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)
(marginalia-mode 1)

(require 'consult)
(consult-customize consult-buffer :preview-key "C-x b")
(global-set-key (kbd "C-x b") #'consult-buffer)
(consult-customize consult-project-buffer :preview-key "C-x p b")
(global-set-key (kbd "C-x p b") #'consult-project-buffer)
(consult-customize consult-mark :preview-key "M-g m")
(consult-customize consult-imenu :preview-key "M-g i")

(consult-customize
 consult--source-buffer
 consult--source-bookmark consult--source-recent-file
 consult--source-project-recent-file :preview-key (kbd "M-."))

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
(setq insert-directory-program "/opt/homebrew/bin/gls")
(setq delete-by-moving-to-trash t)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-u +") 'dired-create-empty-file))

(if (display-graphic-p)
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(require 'command-log-mode)

(require 'diminish)

(require 'which-key)
(which-key-mode 1)
(diminish 'which-key-mode)
(setq which-key-idle-delay 0.3)

;; vc

(require 'git-gutter)
(global-git-gutter-mode 1)

(require 'magit)
(define-key magit-section-mode-map (kbd "C-<tab>") nil)
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

;; shell

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(require 'vterm)
(setq vterm-max-scrollback 10000)

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

(require 'eshell-git-prompt)

;; languages

;; (require 'flycheck)
;; (global-flycheck-mode 1)

(require 'flymake)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
(add-hook 'emacs-lisp-mode-hook 'flymake-mode)

;;; eglot

;;; dape
;; (setq exec-path (cons "~/netcoredbg/bin" exec-path))
;; (add-to-list 'dape-configs
;;              `(unity modes
;;                      (csharp-mode csharp-ts-mode)
;;                      ensure dape-ensure-command command "~/.vscode/extensions/p1gd0g.unity-debug-301-4.0.1/bin/UnityDebug.exe" :request "launch" command-args
;;                      ["--interpreter=vscode"]
;;                      :cwd dape-cwd :program dape--netcoredbg-program :stopAtEntry nil))


(add-hook 'csharp-mode-hook 'eglot-ensure)
(with-eval-after-load 'eglot
  (setq eglot-sync-connect nil)
  (add-to-list 'eglot-server-programs
               '(csharp-mode . ("csharp-ls"))))

(setq-default tab-width 4)
;(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
;(add-hook 'solidity-mode-hook 'eglot-ensure)
;(add-hook 'sh-mode-hook 'eglot-ensure)
;(add-to-list 'eglot-server-programs
;             `(solidity-mode . ("solc" "--lsp")))
;(add-to-list 'eglot-server-programs
                                        ;             `(csharp-mode . ("~/omnisharp-osx-arm64-net6.0/OmniSharp" "-lsp")))
(global-set-key (kbd "C-c e r") #'eglot-reconnect)

;;; corfu

(require 'corfu)
(global-corfu-mode)
(corfu-popupinfo-mode)
(corfu-echo-mode)
(corfu-history-mode)
;; (setq corfu-popupinfo-delay 0.1)
;; (setq corfu-auto-delay 0.1)
;; (setq corfu-auto t)
;; (setq corfu-auto-prefix 2)
(define-key corfu-map (kbd "C-m") nil)

;; (require 'kind-icon)
;; (setq kind-icon-default-face 'corfu-default)
;; (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

;; Enable Completion Preview mode in code buffers
(add-hook 'prog-mode-hook #'completion-preview-mode)
;; also in text buffers
(add-hook 'text-mode-hook #'completion-preview-mode)
;; and in \\[shell] and friends
(with-eval-after-load 'comint
  (add-hook 'comint-mode-hook #'completion-preview-mode))

(with-eval-after-load 'completion-preview
  ;; Show the preview already after two symbol characters
  (setq completion-preview-minimum-symbol-length 2)

  ;; Non-standard commands to that should show the preview:

  ;; Org mode has a custom `self-insert-command'
  (push 'org-self-insert-command completion-preview-commands)
  ;; Paredit has a custom `delete-backward-char' command
  (push 'paredit-backward-delete completion-preview-commands)

  ;; Bindings that take effect when the preview is shown:

  ;; Cycle the completion candidate that the preview shows
  (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate)
  ;; Convenient alternative to C-i after typing one of the above
  (keymap-set completion-preview-active-mode-map "M-i" #'completion-preview-insert))

;;; cape

;; (require 'cape)
;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;; (add-to-list 'completion-at-point-functions #'cape-file)

;;; company

;; (require 'company)
;; (setq company-global-modes '(not vterm-mode))
;; (setq company-keywords-ignore-case t)
;; (global-company-mode)
;; (define-key company-mode-map (kbd "<tab>") 'company-complete-selection)
;; (define-key company-active-map (kbd "C-m") nil)
;; (define-key company-active-map (kbd "C-SPC") #'company-complete-selection)

;; (setq company-minimum-prefix-length 1)
;; (setq company-idle-delay 0.2)
;; (setq company-show-quick-access t)
;; (setq company-backends '(company-elisp company-capf))

;; (require 'company-box)
;; (add-hook 'company-mode-hook 'company-box-mode)

(eldoc-add-command 'c-electric-paren)

(require 'evil-nerd-commenter)
(global-set-key (kbd "C-c C-c") 'evilnc-comment-or-uncomment-lines)

;;; common lisp
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;;; slime

;; (require 'slime)

;; (require 'slime-autoloads)
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (setq common-lisp-style-default "sbcl")
;; (slime-setup '(slime-fancy slime-repl slime-scratch slime-trace-dialog slime-cl-indent slime-cape))

;;; sly
(require 'sly)

(defun enable-sly-tramp ()
  (interactive)
  (add-to-list 'sly-filename-translations (sly-create-filename-translator :machine-instance (sly-machine-instance)
                                                                          :remote-host "ccg-server"
                                                                          :username "root")))

(defun disable-sly-tramp ()
  (interactive)
  (setq sly-filename-translations (remove (nth (1- (length sly-filename-translations)) sly-filename-translations) sly-filename-translations)))

;(add-to-list 'tramp-default-method-alist '("" "root" "ssh"))
;(customize-set-variable 'tramp-default-method "ssh")

;(load "~/quicklisp/log4sly-setup.el")
;(global-log4sly-mode 1)

;; (global-set-key (kbd "C-c s") #'slime-selector)

(global-set-key (kbd "C-M-S-j") 'backward-sexp)
(global-set-key (kbd "C-M-:") 'forward-sexp)
(global-set-key (kbd "C-M-S-k") #'(lambda () (interactive) (re-search-backward "[()]")))
(global-set-key (kbd "C-M-S-l") #'(lambda () (interactive) (re-search-forward "[()]")))

;;; eww

(setq
 browse-url-browser-function 'eww-browse-url ; Use eww as the default browser
 shr-use-fonts  nil                          ; No special fonts
 shr-use-colors nil                          ; No colours
 shr-indentation 2                           ; Left-side margin
 shr-width 70                                ; Fold text to 70 columns
 eww-search-prefix "https://wiby.me/?q=")    ; Use another engine for searching

;;; latex

;;; medium

;;; org

(require 'org)
(add-hook 'org-mode-hook #'(lambda () (setq truncate-lines nil)))
(setq org-default-notes-file (concat org-directory "/notes.org"))

;;; (define-key org-mode-map (kbd "C-,") 'org-delete-backward-char)

(defun create-card ()
  "Create a new card."
  (interactive)
  (let* ((file-name (read-file-name "Enter file name: "))
         (inheritance (read-string "Enter inherited classes: "))
         (card-name (file-name-sans-extension (file-name-nondirectory file-name))))
    (find-file file-name)
    (insert (format ";;;; %s\n\n" card-name))
    (insert "(in-package :tinka.ccg.cards)")
    (insert "\n\n")
    (insert (format "(defcard %s (%s) ("  card-name inheritance))
    (insert ":data-id ")
    (insert "(defdata-id)")
    (sly-macroexpand-1-inplace)
    (insert "\n")
    (insert ":rarity :common\n:cost ()\n:display-name \"\"\n:description \"\")\n)")
    (indent-region 0 (point-max))))

;; (global-set-key (kbd "C-c c") #'create-card)

(require 'server)
(unless (server-running-p)
  (server-start))

(defun my-emacsclient-foreground ()
  "Bring Emacs to the foreground using AppleScript."
  (start-process "emacs-foreground" nil "osascript" (expand-file-name "~/bring_emacs_to_foreground.applescript")))

(add-hook 'server-visit-hook 'my-emacsclient-foreground)

;; (require 'copilot)
;; (add-hook 'prog-mode-hook 'copilot-mode)
;; (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;; (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
;; (add-to-list 'copilot-indentation-alist '(csharp-mode 4))
;; (setq copilot-idle-delay 10000000)
;; (global-set-key (kbd "C-c c") #'(lambda ()
;;                                   (interactive)
;;                                   (corfu-quit)
;;                                   (copilot-complete)))

(require 'embark)
(define-key input-decode-map (kbd "C-[") [Control-bracket])
(global-set-key [Control-bracket] 'embark-act)

(require 'embark-consult)

(require 'vundo)
(setq vundo-glyph-alist vundo-unicode-symbols)

(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(setq ediff-split-window-function 'split-window-horizontally)


(require 'gnus)
(setq send-mail-function #'smtpmail-send-it)

(provide 'init)
;;; init.el ends here
