;;; Compatible with Emacs 24.3 and above

;;; Cask / Pallet boilerplate

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)


;;; Random key bindings
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)


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

;; Visualize the undo tree
(undo-tree-mode 1)

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


;;; Save last position in buffer
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "saved-places"))


;;; Key chords
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.05)
(key-chord-define-global "jd" 'ace-jump-to-definition)
(key-chord-define-global "jg" 'ace-jump-to-git-grep)
(key-chord-define-global "hh" 'helm-swoop)
(key-chord-define-global "jj" 'ace-jump-word-mode)
(key-chord-define-global "jk" 'ace-jump-char-mode)
(key-chord-define-global "jl" 'ace-jump-line-mode)
(key-chord-define-global "uu" 'undo-tree-visualize)
(key-chord-define-global "xf" 'helm-projectile-find-file)
(key-chord-define-global "xg" 'helm-git-grep-at-point)
(key-chord-define-global "xh" 'helm-projectile)
(key-chord-define-global "xn" 'helm-projectile-find-file-in-known-projects)
(key-chord-define-global "xx" 'helm-M-x)


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


;;; NeoTree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)

;;; Projectile
(projectile-global-mode)
(helm-projectile-on)
(setq projectile-completion-system 'helm
      projectile-switch-project-action 'helm-projectile
      projectile-use-git-grep t
      projectile-mode-line '(:eval
                             (format " Prj[%s]" (projectile-project-name)))
      )


;;; Magit
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-c g") 'magit-status)


;;; Python
(elpy-enable)
;; Elpy should use flycheck instead of flymake
(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
(add-hook 'elpy-mode-hook 'flycheck-mode)


;;; Go
(defun go-run-buffer()
  (interactive)
  (save-buffer)
  (shell-command (concat "go run " (buffer-file-name))))
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook '(lambda ()
  (local-set-key (kbd "C-c C-c") 'go-run-buffer)
  (local-set-key (kbd "C-c i") 'go-goto-imports)
  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
  (local-set-key (kbd "C-c C-k") 'godoc)
  (local-set-key (kbd "C-c C-f") 'gofmt)

  ;;; TODO pick path based on projectile project of last golang buffer
  (setenv "GOPATH" "/home/dev/src/openshift:/home/dev/src/origin/Godeps/_workspace")
  ;;(setenv "GOPATH" "/home/dev/src/kubernetes:/home/dev/src/kubernetes/Godeps/_workspace")

  (add-to-list 'load-path (concat (getenv "GOPATH")
                                  "/src/github.com/dougm/goflymake"))
  (add-to-list 'load-path (concat (getenv "GOPATH")
                                  "/src/github.com/nsf/gocode/emacs-company"))

  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)
  (company-go)

  ;;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
                      "go generate && go build -v && go test -v && go vet"))
  (require 'go-projectile)
  ))

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
    (cond
     ((string= major-mode "go-mode") (call-interactively 'godef-jump))
     ((string= major-mode "emacs-lisp-mode") (call-interactively 'find-function))
     (t (message (format "No jump-to-definition function defined for '%s'." major-mode)))))
  (setq ajd/jumping nil))

(add-hook 'ace-jump-mode-before-jump-hook #'ajd/maybe-jump-start)
(add-hook 'ace-jump-mode-end-hook #'ajd/maybe-jump-end)


;;; ace-jump-to-git-grep
(defvar ajg/jumping nil
  "Internal flag for detecting if currently jumping to definition.")

(defun ace-jump-to-git-grep ()
  "Call `ace-jump-word-mode' and launch a  'jump-to-git-grep' function."
  (interactive)
  (let ((ace-jump-mode-scope 'window)))
    (setq ajg/jumping t)
    (call-interactively 'ace-jump-word-mode))

(defun ajg/maybe-jump-start ()
  "Push the mark when jumping to git-grep with `ace-jump-char-mode'."
  (when ajg/jumping
    (push-mark)))

(defun ajg/maybe-jump-end ()
  "Jump to git-grep after jumping with `ace-jump-word-mode.'."
  (when ajg/jumping (call-interactively 'helm-git-grep-at-point))
  (setq ajg/jumping nil))

(add-hook 'ace-jump-mode-before-jump-hook #'ajg/maybe-jump-start)
(add-hook 'ace-jump-mode-end-hook #'ajg/maybe-jump-end)
