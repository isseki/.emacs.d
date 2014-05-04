(require 'cl-lib)

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
(defun monospaced-font ()
  (let ((font-list (list "PC-98 Fixed Font"
			 "Ricty-14")))
    (cl-loop for f in font-list
	     when (find-font (font-spec :name f))
	     return f)))
(defun set-default-font ()
  (when (monospaced-font)
    (set-frame-font (monospaced-font))
    (add-hook 'after-make-frame-functions
	      (lambda (&rest frame)
		(set-frame-font (monospaced-font) t (list (car frame)))))))
(set-default-font)

;; Stop beep sound
(setq ring-bell-function 'ignore)

;; Set home directory
(defvar user-home-directory (expand-file-name "~"))
(setq default-directory user-home-directory)

;; Set load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Key
(global-set-key (kbd "C-h") 'delete-backward-char)

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

;; paredit
(require-package 'paredit)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)

;; paren-mode
(show-paren-mode 1)

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
(global-set-key (kbd "C-c h") 'helm-mini)

;; shell
(when (eq system-type 'windows-nt)
  (setq shell-file-name "C:/MinGW/msys/1.0/bin/bash")
  (setq explicit-shell-file-name shell-file-name)
  (setenv "PATH"
      (concat ".:/usr/local/bin:/mingw/bin:/bin:"
          (replace-regexp-in-string " " "\\\\ "
              (replace-regexp-in-string "\\\\" "/"
                  (replace-regexp-in-string "\\([A-Za-z]\\):" "/\\1"
                      (getenv "PATH")))))))

;; shell-pop
(require-package 'shell-pop)
(global-set-key (kbd "C-,") 'shell-pop)

;; SLIME
(require-package 'slime)
(slime-setup '(slime-repl))
(setq inferior-lisp-program "sbcl")
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
