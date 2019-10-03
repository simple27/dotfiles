;; id
(setq user-full-name "Jonathan Sparling")

;; repos/package handling
(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa")))

;; Bootstrap 'use-package'
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

;; helm setup
(use-package helm-misc)
(use-package helm-projectile)
(use-package helm-locate)
(use-package helm-config
  :config
  (setq helm-quick-update t)
  (setq helm-bookmark-show-location t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-swoop-pre-input-function (lambda () ""))
  )

;; evil and ess
(use-package evil
  :load-path "~/.emacs.d/elpa/evil-1.0.8"
  :config
  ;; enable evil
  (evil-mode 1)

  ;; cursor colors for evil modes
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow))

  ;; key bindings
  (define-key evil-normal-state-map "Za" 'helm-apropos)
  (define-key evil-normal-state-map "\\" 'helm-mini)
  (define-key evil-normal-state-map "|" 'helm-find-files)
  (define-key evil-normal-state-map "Zg" 'helm-google-suggest)
  (define-key evil-normal-state-map "Zh" 'helm-M-x)
  (define-key evil-normal-state-map "Zi" 'helm-semantic-or-imenu)
  (define-key evil-normal-state-map "?" 'helm-etags-select)
  (define-key evil-normal-state-map "U" 'helm-show-kill-ring)
  (define-key evil-normal-state-map "Zm" 'helm-all-mark-rings)
  (define-key evil-normal-state-map "Zo" 'helm-occur)
  (define-key evil-normal-state-map "Zp" 'helm-list-emacs-process)
  (define-key evil-normal-state-map "Zr" 'helm-register)
  (define-key evil-normal-state-map "/" 'helm-multi-swoop-all)
  (define-key evil-normal-state-map "Zt" 'helm-top)
  (define-key evil-normal-state-map "ZX" 'helm-M-x)
  (define-key evil-normal-state-map "QV" 'split-window-right)
  (define-key evil-normal-state-map "QH" 'split-window-below)
  (define-key evil-normal-state-map "QW" 'delete-window)
  (define-key evil-normal-state-map "QO" 'next-multiframe-window)
  (define-key evil-normal-state-map "!!" 'linum-mode)
  (define-key evil-normal-state-map " " 'magit-status)
  )

(use-package ess-site
  :load-path "~/.emacs.d/ess-13.09-1/lisp")

;; powerline
(use-package powerline
  :config
  (powerline-evil-vim-color-theme)
  (display-time-mode t)
  )

;; fix indentation
(use-package dtrt-indent
  :config
  (dtrt-indent-mode 1)
  (define-key global-map (kbd "RET") 'newline-and-indent)
  )

;; 100 column rule and whitespace highlighting
(use-package whitespace
  :config
  (setq whitespace-style '(face empty tabs lines-tail trailing))
  (setq whitespace-line-column 100)
  (global-whitespace-mode t)
  )

;; haskell and agda modes
(use-package flycheck)
(use-package haskell-mode)
(use-package agda-input
  :load-path "~/.emacs.d/emacs-mode"
  :config
  (add-hook 'haskell-mode-hook (lambda () (set-input-method 'Agda)))
  )

;; smooth scrolling
(setq scroll-margin 5 scroll-conservatively 9999 scroll-step 1)

;; stop cluttering directories with backups
(setq make-backup-files nil)

;; better frame titles
(setq frame-title-format (concat  "%b - emacs@" (system-name)))

;; unified diff
(setq diff-switches "-u")

;; easy visible stuff last, so errors are noticeable
(show-paren-mode t) ;; show matching parens
(tool-bar-mode -1)
(menu-bar-mode -1)
(linum-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
