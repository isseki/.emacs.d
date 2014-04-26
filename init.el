;; Don't show up splash page
(setq inhibit-startup-message t)
(when (fboundp 'menu-bar-mode) (menu-bar-mode (if (display-graphic-p) 1 -1)))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))

;; Set up appearance
(add-to-list 'default-frame-alist '(background-color . "black"))
(add-to-list 'default-frame-alist '(foreground-color . "white"))
(add-to-list 'default-frame-alist '(cursor-color . "green"))
(add-to-list 'default-frame-alist '(alpha . 85))

;; Stop beep sound
(setq ring-bell-function 'ignore)

;; Set load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(defun require-package (package)
  (unless (package-installed-p package)
    (package-install package)))

;; evil
(require-package 'evil)
(evil-mode 1)
(define-key evil-normal-state-map (kbd "TAB") 'indent-according-to-mode)
(require-package 'surround)
(global-surround-mode 1)

;; smartparens
(require-package 'smartparens)
(smartparens-global-mode t)
(add-hook 'prog-mode-hook
	  (lambda () (turn-on-smartparens-strict-mode)))
(show-smartparens-global-mode t)

;; highlight-parentheses
(require-package 'highlight-parentheses)
(add-hook 'prog-mode-hook
	  (lambda () (highlight-parentheses-mode t)))

;; hl-line
(require-package 'hl-line)
(global-hl-line-mode 1)
(set-face-background 'highlight "#222222")

