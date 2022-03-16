;; Ensure automatic generation of compiled files
(setq comp-deferred-compilation t)

;; Ensure lsp uses higher-performing plists in preference to hash-tables
(setq lsp-use-plists t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes '(tsdh-light))
 '(dired-vc-rename-file t)
 '(git-messenger:show-detail t)
 '(git-messenger:use-magit-popup t)
 '(markdown-command "/usr/bin/multimarkdown")
 '(package-selected-packages
   '(dap-mode ein hyperbole speed-type json-mode helm-lsp vterm-toggle vterm git-messenger kubel ob-rust ob-go w3m rustic helm-rg forge helm-git-grep pytest yasnippet-snippets helm-c-yasnippet bind-key dash-functional treepy lsp-python-ms use-package go-playground lsp-ui helm-company lsp-mode flycheck-golangci-lint go-snippets transient ace-jump-mode ghub logito pkg-info popup go-dlv epl marshal pcache pyvenv async auto-complete cask company dash f find-file-in-project flycheck gh git-commit go-eldoc go-mode go-rename helm helm-core highlight-indentation ht ivy magit-popup package-build projectile s with-editor yasnippet gotest git-link yaml-mode session puppet-mode pbcopy pallet neotree monokai-theme markdown-mode magit key-chord helm-themes helm-swoop helm-projectile helm-flyspell go-projectile go-errcheck go-autocomplete flycheck-pyflakes exec-path-from-shell elpy dockerfile-mode company-go ace-jump-zap))
 '(projectile-enable-caching t)
 '(session-use-package t nil (session))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;; Random key bindings
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-c x") 'compile)
(global-set-key (kbd "C-c c") 'recompile)
(global-set-key (kbd "C-c d") 'dlv)
(global-set-key (kbd "C-c t") 'vterm)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-M-f") 'toggle-fullscreen)


;;; UX niceties

;; Show hostname in the mode line
(let ((pos (memq 'mode-line-modes mode-line-format)))
  (setcdr pos (cons (getenv "HOSTNAME") (cdr pos))))

;; Replace the active region by typing text
(delete-selection-mode 1)

;; Automatic matching pairs
(electric-pair-mode 1)

;; Enable quick confirmation
(fset 'yes-or-no-p 'y-or-n-p)

(load-theme 'monokai t)

;; Disable menu bar in the cli
(unless (display-graphic-p)
  (menu-bar-mode 0))

;; Minimize ui elements
(tool-bar-mode -1)

;; Show column on status line
(setq column-number-mode t)

;; Hide the splash screen
(setq inhibit-splash-screen t)

(setq require-final-newline t)

;; Do not insert tabs in place of multiple spaces
(setq-default indent-tabs-mode nil)

(setq-default tab-width 4)

;; No more trailing whitespace, ever!
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Show matching pairs of special characters
(show-paren-mode 1)

;; Save history across sessions
(add-hook 'after-init-hook 'session-initialize)

;; Ace-jump-zap
(global-set-key (kbd "M-z") 'ace-jump-zap-up-to-char-dwim)
(global-set-key (kbd "C-M-z") 'ace-jump-zap-to-char-dwim)

;; Visual indication of lines > 79 chars
;; Disabled while I work on codebases with no regard for line length.
;; (require 'whitespace)
;; (setq whitespace-line-column 79)
;; (setq whitespace-style '(face lines-tail))
;; (add-hook 'prog-mode-hook 'whitespace-mode)

;; Ensure unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Prefer horizontal split
(setq split-height-threshold 0)
(setq split-width-threshold 0)

;; Display time in the modeline
(display-time-mode 1)

;; Save last position in buffer
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "saved-places"))

;; Ignore files matching save-place-skip-check-regexp
(setq-default save-place-save-skipped nil)


;;; Key chords
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.05)
(key-chord-define-global "hh" 'helm-swoop)
(key-chord-define-global "jd" 'ace-jump-to-definition)
(key-chord-define-global "jg" 'ace-jump-to-helm-rg)
(key-chord-define-global "jj" 'ace-jump-word-mode)
(key-chord-define-global "jk" 'ace-jump-char-mode)
(key-chord-define-global "jl" 'ace-jump-line-mode)
(key-chord-define-global "xf" 'helm-projectile-find-file)
(key-chord-define-global "xg" 'helm-rg)
(key-chord-define-global "xh" 'helm-projectile)
(key-chord-define-global "xo" 'ace-window)
(key-chord-define-global "xx" 'helm-M-x)
(key-chord-define-global "xb" 'switch-to-buffer)


;;; Enable backup files.
(setq make-backup-files t)
;; Automatically delete excess backups
(setq delete-old-versions t)
;; Enable versioning with default values
(setq version-control t)
;; Save all backup file in this directory.
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))


;;; git-messenger
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)


;;; company-mode
(setq company-selection-wrap-around t)


;;; Helm
(require 'helm-config)
(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c s 1") 'helm-git-grep-at-point)
(global-set-key (kbd "C-c s 2") 'helm-occur)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(setq helm-ff-skip-boring-files t)
(setq helm-ff-file-name-history-use-recentf t)
; Open helm buffer inside current window, not occupy whole other window
(setq helm-split-window-in-side-p t)
; Move to end or beginning of source when reaching top or bottom of source.
(setq helm-move-to-line-cycle-in-source t)
; Search for library in `require' and `declare-function' sexp.
(setq helm-ff-search-library-in-sexp t)
; Scroll 8 lines other window using M-<next>/M-<prior>
(setq helm-scroll-amount 8)


;;; Hyperbole
(load "hyperbole-autoloads")
(load "hyperbole")


;;; helm-rg
(setq helm-rg-default-directory 'git-root)


;;; NeoTree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)


;;; org-mode

; Workaround iterm2 shell handling
(define-key input-decode-map "\e[1;10A" [M-S-up])
(define-key input-decode-map "\e[1;10B" [M-S-down])
(define-key input-decode-map "\e[1;10C" [M-S-right])
(define-key input-decode-map "\e[1;10D" [M-S-left])
(define-key input-decode-map "\e[1;9A" [M-up])
(define-key input-decode-map "\e[1;9B" [M-down])
(define-key input-decode-map "\e[1;9C" [M-right])
(define-key input-decode-map "\e[1;9D" [M-left])

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (go . t)
   (rust . t)
   (shell . t)
   ))


;;; Projectile
(projectile-global-mode)
(helm-projectile-on)
(setq projectile-completion-system 'helm
      projectile-switch-project-action 'helm-projectile
      projectile-use-git-grep t
      projectile-mode-line '(:eval
                             (format " Prj[%s]" (projectile-project-name)))
      )
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;;; Magit
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-c g") 'magit-status)
(use-package forge
    :after magit)


;;; LSP
(use-package lsp-mode
  :commands lsp
  :hook
  (go-mode . lsp)
  (python-mode . lsp)
  (lsp-mode . lsp-ui)
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all nil)
  (lsp-ui-doc-enable nil)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints nil)
  (lsp-headerline-bre0adcrumb-enable nil)
  ;; (lsp-enable-file-watchers nil)
  (lsp-diagnostic-package :none)
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(global-set-key (kbd "C-c C-e") 'flymake-goto-next-error)

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;; Set gc threshold higher to account for lsp's client/server communication overhead
(setq gc-cons-threshold 100000000)

;; LSP responds are in hundreds of not thousands of kb
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq lsp-log-io nil) ; if set to true can cause a performance hit


;; dap
(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(use-package dap-mode
  :ensure
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)
  (dap-mode 1)
  (setq dap-print-io t)

  (require 'dap-go)
  (dap-go-setup)

  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)

  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
	     :gdbpath "rust-lldb"
         :target nil
         :cwd nil)))


;;; yasnippet
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))


;;; Python
(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp-deferred)
                          (local-set-key (kbd "C-c C-t") 'pytest-one)
                          ;; This key binding is used globally for jump-to-definition
                          (local-unset-key (kbd "C-c C-j"))
                          (local-set-key (kbd "C-c r") (kbd "C-u C-c C-c") ))))
(setq python-shell-interpreter "python3")


;;; Go
(add-hook 'before-save-hook 'gofmt-before-save)

;; Provide a reasonable default for kubeconfig
(setenv "KUBECONFIG" (concat (getenv "HOME") "/.kube/default-kubeconfig"))

;; Given a buffer name, set the appropriate GOPATH.  If the buffer
;; name indicates a go project (i.e. <path>/src/<project id>/src), use
;; the root of the project (i.e. <path>/src/<project id>).  Otherwise,
;; use '$HOME/go'.
(defun maru-set-gopath-from-buffer-name (buffer-name)
  (let ((path-entries (split-string buffer-name "/"))
        (gopath "")
        (i 0))
    (progn
      (while (and
              (string= "" gopath)
              (<= i (length path-entries)))
        ;; If "src" is found
        (if (string= "src" (nth i path-entries))
            ;; If the nth + 2 element is also "src"
            (if (and (<= (+ i 2) (length path-entries))
                     (string= "src" (nth (+ i 2) path-entries)))
                ;; Set the gopath to the nth + 1 element
                (setq gopath (string-join (seq-take path-entries (+ i 2)) "/"))))
        (setq i (1+ i)))
      (if (string= "" gopath )
          (setq gopath (concat (getenv "HOME") "/go")))
      (setenv "GOPATH" gopath))))

(defun maru-go-buffer-list-update-hook ()
  (if (not (null buffer-file-name))
      (maru-set-gopath-from-buffer-name buffer-file-name)))

(add-hook 'buffer-list-update-hook 'maru-go-buffer-list-update-hook)

(defun maru-go-mode-hook ()
  (local-set-key (kbd "C-c f") 'go-test-current-file)
  (local-set-key (kbd "C-c t") 'go-test-current-test)
  (local-set-key (kbd "C-c r") 'lsp-ui-peek-find-references)
  (local-set-key (kbd "C-c C-o") 'lsp-organize-imports)

  ;;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v"))

  (go-guru-hl-identifier-mode)
  (require 'go-projectile)
  )
(add-hook 'go-mode-hook 'maru-go-mode-hook)


;;; Rust
(use-package rustic
  :ensure
  :config
  (setq rustic-format-on-save t))


;;; Javascript
(setq js-indent-level 2)


;; Flycheck
(use-package flycheck :ensure)


;;; Flyspell
;; Fix aspell compatibility
(setq ispell-list-command "--list")
;; Check spelling in comments and doc strings
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
;; Use helm flyspell for correction
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-M-i") 'helm-flyspell-correct))


;;; OS X-specific fixup
(when (equal (symbol-name system-type) "darwin")
  ;; Fix x11 support
  (setq mac-option-modifier 'meta)
  ;; Disable scroll bar
  (toggle-scroll-bar -1)
  ;; Enable copy-paste integration on os x
  (require 'pbcopy)
  (turn-on-pbcopy))


;;; Shell
(add-hook 'sh-mode-hook '(lambda ()
  (setq sh-basic-offset 2)
  (setq sh-indentation 2)
  ))


;;; ace-jump-to-definition
(defvar ajd/jumping nil
  "Internal flag for detecting if currently jumping to definition.")

(defun ace-jump-to-definition ()
  "Call `ace-jump-word-mode' and launch a  'jump-to-definition' function."
  (interactive)
  (let ((ace-jump-mode-scope 'window)))
    (setq ajd/jumping t)
    (call-interactively 'ace-jump-word-mode))

(defun ajd/maybe-jump-start ()
  "Push the mark when jumping to definition with `ace-jump-word-mode'."
  (when ajd/jumping
    (push-mark)))

(defun ajd/maybe-jump-end ()
  "Jump to definition after jumping with `ace-jump-word-mode.'."
  (when ajd/jumping
    (cond
     ((string= major-mode "emacs-lisp-mode") (call-interactively 'xref-find-definitions))
     (t (call-interactively 'lsp-find-definition))))
  (setq ajd/jumping nil))

(add-hook 'ace-jump-mode-before-jump-hook #'ajd/maybe-jump-start)
(add-hook 'ace-jump-mode-end-hook #'ajd/maybe-jump-end)


;;; ace-jump-to-rg
(defvar ajg/jumping nil
  "Internal flag for detecting if currently jumping to definition.")

(defun ace-jump-to-helm-rg ()
  "Call `ace-jump-word-mode' and launch a  'jump-to-helm-rg' function."
  (interactive)
  (let ((ace-jump-mode-scope 'window)))
    (setq ajg/jumping t)
    (call-interactively 'ace-jump-word-mode))

(defun ajg/maybe-jump-start ()
  "Push the mark when jumping to helm-rg with `ace-jump-char-mode'."
  (when ajg/jumping
    (push-mark)))

(defun ajg/maybe-jump-end ()
  "Jump to helm-rg after jumping with `ace-jump-word-mode.'."
  (when ajg/jumping (call-interactively 'helm-rg))
  (setq ajg/jumping nil))

(add-hook 'ace-jump-mode-before-jump-hook #'ajg/maybe-jump-start)
(add-hook 'ace-jump-mode-end-hook #'ajg/maybe-jump-end)


;;; Enable fullscreen toggle for macos
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
