;; id
(setq user-full-name "Jonathan Sparling")

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diff
(setq diff-switches "-u")

;; set up repos/package handling
(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "http://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("org" . "http://orgmode.org/elpa")))
(add-to-list 'load-path "~/.emacs.d/ess-13.09-1/lisp")
(add-to-list 'load-path "~/.emacs.d/elpa/evil-1.0.8")

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
(tool-bar-mode -1)

;; after macro
;; "after" macro definition
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
(define-key evil-normal-state-map "Zb" 'helm-mini)
(define-key evil-normal-state-map "Zf" 'helm-find-files)
(define-key evil-normal-state-map "Zg" 'helm-google-suggest)
(define-key evil-normal-state-map "Zh" 'helm-M-x)
(define-key evil-normal-state-map "Zk" 'helm-show-kill-ring)
(define-key evil-normal-state-map "Zm" 'helm-all-mark-rings)
(define-key evil-normal-state-map "Zo" 'helm-occur)
(define-key evil-normal-state-map "Zp" 'helm-list-emacs-processes)
(define-key evil-normal-state-map "Zr" 'helm-register)
(define-key evil-normal-state-map "Zs" 'helm-semantic-or-imenu)
(define-key evil-normal-state-map "Zt" 'helm-top)
(define-key evil-normal-state-map "Zw" 'helm-surfraw)

;; aesthetics
(custom-set-variables
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (whiteboard))))
(custom-set-faces)

;; smooth scrolling
(setq scroll-margin 5
scroll-conservatively 9999
scroll-step 1)

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
