;; packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
;;                       ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;; evil
(require 'evil)
(evil-mode 1)
(setq evil-move-cursor-back nil) ;; do not move back on ESC

;; ui changes
(setq ring-bell-function 'ignore) ;; disable bell
(tool-bar-mode -1) ;; disable toolbar
(when window-system (set-frame-size (selected-frame) 160 50)) ;; larger size
(setq inhibit-startup-message t) ;; disable welcome message
(scroll-bar-mode -1) ;; disable scrollbar

;; change color of cursor depending on mode
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

;; theme
(load-theme 'hipster t)

;; highlight current line
(global-hl-line-mode 1)

;; font
(set-face-attribute 'default t :font "Consolas-13")

;; indentation
(setq-default tab-width 4 indent-tabs-mode nil)
(define-key global-map (kbd "RET") 'newline-and-indent)

; remember position on reopen file
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;; powerline
(require 'powerline)

(defun powerline-custom-theme ()
  "Custom powerline theme"
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (let ((evil-face (powerline-evil-face)))
                                       (if evil-mode
                                           (powerline-raw (powerline-evil-tag) evil-face)))
                                     (powerline-buffer-id `(mode-line-buffer-id ,mode-line) 'l)
                                     (funcall separator-right mode-line face1)
                                     (powerline-raw " " face1)
                                     (powerline-major-mode face1)
                                     (powerline-process face1)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (when (buffer-modified-p)
                                       (powerline-raw "+" face2))
                                     (when buffer-read-only
                                       (powerline-raw "RO" face2))
                                     (powerline-raw "%z" face2)
                                     (funcall separator-left face2 mode-line)
                                     (powerline-raw " " mode-line)
                                     ;; (powerline-raw (concat "[" (mode-line-eol-desc) "]") mode-line)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-minor-modes mode-line)
                                     (powerline-raw "%n " mode-line)
                                     (when (and vc-mode buffer-file-name)
                                       (let ((backend (vc-backend buffer-file-name)))
                                         (when backend
                                           (concat (powerline-raw "[" mode-line 'l)
                                                   (powerline-raw (format "%s / %s" backend (vc-working-revision buffer-file-name backend)))
                                                   (powerline-raw "]" mode-line)))))))
                          (rhs (list (powerline-raw '(10 "%i"))
                                     (powerline-raw global-mode-string mode-line 'r)
                                     (powerline-raw "%l," mode-line 'l)
                                     (powerline-raw (format-mode-line '(10 "%c")))
                                     (powerline-raw (replace-regexp-in-string  "%" "%%" (format-mode-line '(-3 "%p"))) mode-line 'r))))
                     (concat (powerline-render lhs)
                             (powerline-fill mode-line (powerline-width rhs))
                             (powerline-render rhs)))))))

(set-face-attribute 'powerline-active1 nil :background "#1179BA" :foreground "#ffffff" :box nil)
(set-face-attribute 'powerline-inactive1 nil :box nil)
(set-face-attribute 'powerline-active2 nil :background "#E37F2D" :foreground "#ffffff" :box nil)
(set-face-attribute 'powerline-inactive2 nil :box nil)
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-attribute 'mode-line-highlight nil)
(powerline-custom-theme)

;; smoother scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-margin 50)

;; relative numbering
(add-hook 'prog-mode-hook 'relative-line-numbers-mode t)
(add-hook 'prog-mode-hook 'line-number-mode t)
(add-hook 'prog-mode-hook 'column-number-mode t)

;; save desktop by default
;; (desktop-save-mode 1)

;; enable ERC irc client
(require 'erc)

(add-to-list 'erc-mode-hook (lambda ()
                              (set (make-local-variable 'scroll-conservatively) 101)
                              (set (make-local-variable 'scroll-margin) 0)))

;; colorize nicks
(require 'erc-hl-nicks)

;; helm
;(require 'helm-config)
;(helm-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("ed5af4af1d148dc4e0e79e4215c85e7ed21488d63303ddde27880ea91112b07e" "f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" "a507b9ca4a605d5256716da70961741b9ef9ec3246041a4eb776102e8df18418" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
