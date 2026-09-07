;;; init.el --- this is my init.el
;;; Commentary: not sure what I'm meant to say here!
;; packages

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)
(setq package-install-upgrade-built-in t)

(defvar packages
  '(gnu-elpa-keyring-update
    all-the-icons
    all-the-icons-completion
    doom-modeline
    ef-themes
    avy
    rainbow-delimiters
    highlight-parentheses
    helpful
    vertico
    orderless
    marginalia
    consult
    pulsar
    hydra
    all-the-icons-dired
    command-log-mode
    diminish
    which-key
    git-gutter
    magit
    exec-path-from-shell
    eshell-git-prompt
    eldoc
    jsonrpc
    ztree
    markdown-mode
    embark
    embark-consult
    restclient
    scala-mode)
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

(setq grep-program "rg")

;; keys

(global-set-key (kbd "C-,") (kbd "<backspace>"))
(global-set-key (kbd "C-<") 'backward-kill-word)
(global-set-key (kbd "C-S-d") 'kill-word)
(global-set-key (kbd "C-S-b") 'backward-word)
(global-set-key (kbd "C-S-f") 'forward-word)
(global-set-key (kbd "C-<tab>") 'tab-next)
(global-set-key (kbd "C-S-<tab>") 'tab-previous)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-;") 'dabbrev-expand)

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
                shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "Fira Mono" :height 140)
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 140)
(set-face-attribute 'variable-pitch nil :font "Hack" :height 160 :weight 'regular)

(require 'all-the-icons)
(unless (find-font (font-spec :name "all-the-icons"))
  (all-the-icons-install-fonts t))

(all-the-icons-completion-mode)
(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)

(mapc #'disable-theme custom-enabled-themes)

(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-height 15)

(require 'ef-themes)
(load-theme 'ef-summer :no-confirm)

(setq avy-keys (number-sequence ?a ?z))
(require 'avy)
(global-set-key (kbd "C-'") 'avy-goto-char-timer)
(setq avy-timeout-seconds 0.25)
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
(setq completion-styles '(orderless basic flex)
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

(setf completion-auto-select t ;; Show completion on first call
      completion-auto-help 'visible ;; Display *Completions* upon first request
      completions-format 'one-column ;; Use only one column
      completions-sort 'historical ;; Order based on minibuffer history
      completions-max-height 20 ;; Limit completions to 15 (completions start at line 5)
      completion-ignore-case t)

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
(setq magit-ediff-dwim-show-on-hunks t)
(setopt magit-git-executable "/opt/homebrew/bin/git")
(setopt magit-diff-refine-hunk t)

(with-eval-after-load 'magit
  (transient-append-suffix 'magit-log "-n"
    '("-m" "No merges" "--no-merges")))

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(exec-path-from-shell-copy-envs '("PATH"
                                  "JAVA_HOME"
                                  "LIBRARY_PATH"
                                  "VULKAN_SDK"
                                  "DYLD_LIBRARY_PATH"
                                  "VK_ICD_FILENAMES"
                                  "VK_DRIVER_FILES"
                                  "VK_ADD_LAYER_PATH"))

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

(with-eval-after-load 'smerge-mode
  (define-key smerge-mode-map (kbd "M-n") 'smerge-next)
  (define-key smerge-mode-map (kbd "M-p") 'smerge-prev))

(setq eglot-server-programs
      '((csharp-mode . ("csharp-ls"))))

;; (setq eglot-server-programs
;;      '((csharp-mode . ("dotnet" "/Users/guillaume.portes/.omnisharp/OmniSharp.dll" "-lsp"))))
;; (setq eglot-connect-timeout 120)

(add-hook 'csharp-mode-hook 'eglot-ensure)
(with-eval-after-load 'eglot
  (setq eglot-sync-connect nil))

(setq-default tab-width 4)

(global-set-key (kbd "C-c e r") #'eglot-reconnect)

;; Enable Completion Preview mode in code buffers
(add-hook 'prog-mode-hook #'completion-preview-mode)
;; also in text buffers
(add-hook 'text-mode-hook #'completion-preview-mode)
;; and in \\[shell] and friends
(add-hook 'eshell-mode-hook #'completion-preview-mode)
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

(eldoc-add-command 'c-electric-paren)

(load "~/emacs/common-lisp.el")

;;; org

(require 'org)
(add-hook 'org-mode-hook #'(lambda () (setq truncate-lines nil)))
(setq org-default-notes-file (concat org-directory "/notes.org"))

(require 'server)
(unless (server-running-p)
  (server-start))

(require 'embark)
(define-key input-decode-map (kbd "C-[") [Control-bracket])
(global-set-key [Control-bracket] 'embark-act)

(require 'embark-consult)

(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(setq ediff-split-window-function 'split-window-horizontally)

(setq ediff-window-setup-function #'ediff-setup-windows-plain)

(unless (package-installed-p 'gptel)
  (package-vc-install "https://github.com/karthink/gptel"))
(require 'gptel)

(unless (package-installed-p 'gptel-agent)
  (package-vc-install "https://github.com/karthink/gptel-agent"))
(require 'gptel-agent)
(require 'gptel-agent-tools)

(global-set-key (kbd "C-z") 'gptel-send)
(gptel-highlight-mode)
(define-globalized-minor-mode gptel-highlight-mode-global
  gptel-highlight-mode
  (lambda () (gptel-highlight-mode)))
(gptel-highlight-mode-global)

(setq epg-pinentry-mode 'loopback)

;; OpenRouter offers an OpenAI compatible API
(setq gptel-backend
      (gptel-make-openai "OpenRouter"               ;Any name you want
        :host "openrouter.ai"
        :endpoint "/api/v1/chat/completions"
        :stream t
        :key (plist-get (car (auth-source-search :host "openrouter.ai" :user "api-key")) :secret)
        :models '(meta/muse-spark-1.2-contributor
                  meta/muse-glimmer-30b
                  tencent/hy3
                  openai/gpt-5.6-luna-pro
                  openai/gpt-5.6-luna
                  openai/gpt-5.6-terra
                  anthropic/claude-sonnet-5
                  z-ai/glm-5.2
                  minimax/minimax-m3
                  deepseek/deepseek-v4-flash-0731
                  deepseek/deepseek-v4-pro-latest
                  xiaomi/mimo-v2.5
                  moonshotai/kimi-k3
                  qwen/qwen3.7-plus
                  google/gemini-3.7-flash:batch
                  stealth/ox-alpha)))

(add-hook 'gptel-post-response-functions 'gptel-end-of-response)
(setopt gptel-model 'openai/gpt-5.6-luna)
(gptel-agent-update)

(gptel-make-preset 'common-lisp
  :description "A preset for Common Lisp help"
  :backend "OpenRouter"
  :model 'mistralai/devstral-2512
  :system "You are an expert Common Lisp coding assistant. Your role is to provide high-quality code solutions, refactorings, and explanations. You are thoughtful and are not trying to please the user. You are happy to be challenged and to challenge. Your priority is to provide idiomatic, well engineered responses. Responses should be succint when possible."
  :tools '("search_web" "read_url"))

(provide 'init)
;;; init.el ends here
