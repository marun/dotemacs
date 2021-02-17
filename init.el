;;; Compatible with Emacs 24.3 and above

;;; Cask / Pallet boilerplate

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(when (version< emacs-version "27.0") (package-initialize))

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes '(tsdh-light))
 '(package-selected-packages
   '(w3m rustic helm-rg forge helm-git-grep lsp-treemacs pytest yasnippet-snippets helm-c-yasnippet bind-key dash-functional treepy lsp-python-ms use-package go-playground lsp-ui helm-company company-lsp lsp-mode flycheck-golangci-lint go-snippets transient ace-jump-mode ghub logito pkg-info popup go-dlv epl marshal pcache pyvenv async auto-complete cask company dash f find-file-in-project flycheck gh git-commit go-eldoc go-mode go-rename helm helm-core highlight-indentation ht ivy magit-popup package-build projectile s with-editor yasnippet gotest git-link yaml-mode session puppet-mode pbcopy pallet neotree monokai-theme markdown-mode magit key-chord helm-themes helm-swoop helm-projectile helm-flyspell go-projectile go-errcheck go-autocomplete flycheck-pyflakes exec-path-from-shell elpy dockerfile-mode company-go ace-jump-zap))
 '(projectile-enable-caching t)
 '(session-use-package t nil (session)))
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


;;; UX niceties

;; Replace the active region by typing text
(delete-selection-mode 1)

;; Automatic matching pairs
(electric-pair-mode 1)

;; Enable quick confirmation
(fset 'yes-or-no-p 'y-or-n-p)

(load-theme 'monokai t)

;; Disable menu bar by default
(menu-bar-mode 0)

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
(require 'whitespace)
(setq whitespace-line-column 79)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Ensure unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Prefer horizontal split
(setq split-height-threshold 0)
(setq split-width-threshold 0)

;; Display time in the modeline
(display-time-mode 1)

;;; Save last position in buffer
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
(key-chord-define-global "xn" 'helm-projectile-find-file-in-known-projects)
(key-chord-define-global "xr" 'rustic-popup)
(key-chord-define-global "xo" 'ace-window)
(key-chord-define-global "xx" 'helm-M-x)
(key-chord-define-global "xb" 'switch-to-buffer)
(key-chord-define-global "x0" 'delete-window)
(key-chord-define-global "x1" 'delete-other-windows)
(key-chord-define-global "x2" 'split-window-below)
(key-chord-define-global "x3" 'split-window-right)
(key-chord-define-global "xy" 'yank)


;;; Enable backup files.
(setq make-backup-files t)
;; Automatically delete excess backups
(setq delete-old-versions t)
;; Enable versioning with default values
(setq version-control t)
;; Save all backup file in this directory.
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))


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
  :hook
  (go-mode . lsp)
  (python-mode . lsp)
  :config
  (setq lsp-eldoc-render-all nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-diagnostic-package :none)
  :commands lsp)
(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(global-set-key (kbd "C-c C-e") 'flymake-goto-next-error)


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

;; Default to using vendoring until go1.14 enables detection
(setenv "GOFLAGS" "-mod=vendor")

;; Provide a reasonable default for kubeconfig
(setenv "KUBECONFIG" "~/.kube/default-kubeconfig")

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

  ;; Prefer universal-jump-to-definition
  (local-unset-key (kbd "C-c C-j"))

  ;;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v"))

  (go-guru-hl-identifier-mode)
  (require 'go-projectile)
  )
(add-hook 'go-mode-hook 'maru-go-mode-hook)


;;; Rust
(use-package rustic)
(setq rustic-format-trigger 'on-save)


;;; Javascript
(setq js-indent-level 2)


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
  ;; Enable copy-paste integration on os x
  (require 'pbcopy)
  (turn-on-pbcopy))


;;; Shell
(add-hook 'sh-mode-hook '(lambda ()
  (setq sh-basic-offset 2)
  (setq sh-indentation 2)
  ))


;;; Common function for triggering jump-to-definition
(defun universal-jump-to-definition ()
  (cond
   ((string= major-mode "go-mode") (call-interactively 'lsp-find-definition))
   ((string= major-mode "emacs-lisp-mode") (call-interactively 'find-function))
   ((string= major-mode "python-mode") (call-interactively 'lsp-find-definition))
   ((string= major-mode "rustic-mode") (call-interactively 'lsp-find-definition))
   (t (message (format "No jump-to-definition function defined for '%s'." major-mode)))))
(global-set-key (kbd "C-c C-j") (lambda () (interactive) (universal-jump-to-definition)))


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
  "Push the mark when jumping to definition with `ace-jump-char-mode'."
  (when ajd/jumping
    (push-mark)))

(defun ajd/maybe-jump-end ()
  "Jump to definition after jumping with `ace-jump-word-mode.'."
  (when ajd/jumping
    (universal-jump-to-definition))
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
