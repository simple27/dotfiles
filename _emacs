;; id
(setq user-full-name "Jonathan Sparling")

;; better frame titles
(setq frame-title-format (concat  "%b - emacs@" (system-name)))

;; unified diff
(setq diff-switches "-u")

;; repos/package handling
(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa")))

(add-to-list 'load-path "~/.emacs.d/ess-13.09-1/lisp")
(add-to-list 'load-path "~/.emacs.d/elpa/evil-1.0.8")
(add-to-list 'load-path "~/.emacs.d/emacs-mode")
(add-to-list 'load-path "~/.emacs.d/dante")
(add-to-list 'load-path "~/.emacs.d/f")
(add-to-list 'load-path "~/.emacs.d/s")
(add-to-list 'load-path "~/.emacs.d/lcr")

(defun require-package (package)
  (setq-default highlight-tabs t)
   "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;; evil and ess
(require 'evil)
(require 'ess-site)
(evil-mode 1)

;; after macro
(if (fboundp 'with-eval-after-load)
    (defmacro after (feature &rest body)
      "After FEATURE is loaded, evaluate BODY."
      (declare (indent defun))
      `(with-eval-after-load ,feature ,@body))
  (defmacro after (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

;; cursor colors for evil modes
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

;; helm setup
(require 'helm-config)
(require 'helm-misc)
(require 'helm-projectile)
(require 'helm-locate)
(setq helm-quick-update t)
(setq helm-bookmark-show-location t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-swoop-pre-input-function (lambda () ""))

(defun helm-my-buffers ()
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
  (helm-other-buffer '(helm-c-source-buffers-list
                       helm-c-source-elscreen
                       helm-c-source-projectile-files-list
                       helm-c-source-ctags
                       helm-c-source-recentf
                       helm-c-source-locate)
                     "*helm-my-buffers*")))

(define-key evil-normal-state-map "Za" 'helm-apropos)
(define-key evil-normal-state-map "\\" 'helm-mini)
(define-key evil-normal-state-map "|" 'helm-find-files)
(define-key evil-normal-state-map "Zg" 'helm-google-suggest)
(define-key evil-normal-state-map "Zh" 'helm-M-x)
(define-key evil-normal-state-map "Zi" 'helm-semantic-or-imenu)
(define-key evil-normal-state-map "?" 'helm-show-kill-ring)
(define-key evil-normal-state-map "Zm" 'helm-all-mark-rings)
(define-key evil-normal-state-map "Zo" 'helm-occur)
(define-key evil-normal-state-map "Zp" 'helm-list-emacs-process)
(define-key evil-normal-state-map "Zr" 'helm-register)
(define-key evil-normal-state-map "/" 'helm-swoop)
(define-key evil-normal-state-map "Zt" 'helm-top)

;; aesthetics
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (whiteboard)))
 '(package-selected-packages
   (quote
    (haskell-mode magit powerline-evil helm-swoop helm-projectile helm-google ess dtrt-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; smooth scrolling
(setq scroll-margin 5 scroll-conservatively 9999 scroll-step 1)

;; powerline
(require 'powerline)
(powerline-evil-vim-color-theme)
(display-time-mode t)

;; fix indentation
(require 'dtrt-indent)
(dtrt-indent-mode 1)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; stop cluttering directories with backups
(setq make-backup-files nil)

;; other bindings
(define-key evil-normal-state-map "ZX" 'execute-extended-command)
(define-key evil-normal-state-map "QV" 'split-window-right)
(define-key evil-normal-state-map "QH" 'split-window-below)
(define-key evil-normal-state-map "QW" 'delete-window)
(define-key evil-normal-state-map "QO" 'next-multiframe-window)
(define-key evil-normal-state-map "!!" 'linum-mode)

;; magit bindings
(define-key evil-normal-state-map " " 'magit-status)

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

;; show matching parens
(show-paren-mode t)

;; 80 column rule, whitespace highlighting
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; easy visible stuff last, so errors are noticeable
;; (scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(linum-mode)

(require 'dante)
(require 'f)
(require 'flycheck)
(require 'lcr)
(require 's)
(require 'haskell-mode)

;;  :ensure t
;;  :after haskell-mode
;;  :commands 'dante-mode
;;  :init
(add-hook 'haskell-mode-hook 'dante-mode)
(add-hook 'haskell-mode-hook 'flycheck-mode)
(setq dante-repl-command-line '("bake" "ghci"))


;; haskell and agda modes
(require 'agda-input)
(add-hook 'haskell-mode-hook (lambda () (set-input-method 'Agda)))
