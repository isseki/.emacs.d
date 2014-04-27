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

;; Font
(defun set-default-font (name)
  (let ((font (find-font (font-spec :name name))))
    (when font
      (set-frame-font font)
      (add-hook 'after-make-frame-functions
		(lambda (&rest frame)
		  (set-frame-font font t (list (car frame))))))))
(set-default-font "PC-98 Fixed Font")

;; Stop beep sound
(setq ring-bell-function 'ignore)

;; Set home directory
(defvar user-home-directory (expand-file-name "~"))
(setq default-directory user-home-directory)

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

;; smooth-scrolling
(require-package 'smooth-scrolling)

;; auto-complete
(require-package 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-show-menu t
      ac-show-menu-immediately-on-auto-complete t
      ac-use-quick-help t
      ac-quick-help-delay 0.2
      ac-quick-help-height 40)

;; helm
(require-package 'helm)
(require 'helm-config)
(setq helm-input-idle-delay 0.01)
(helm-mode 1)
