(load-file "~/.emacs.d/init/packages.el")
(load-file "~/.private.el")

;; projectile

(projectile-global-mode)
(setq projectile-require-project-root nil)

;; evil
(require 'evil)
(evil-mode 1)
(setq evil-move-cursor-back nil) ;; do not move back on ESC
(setq evil-esc-delay 0)

(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "." 'find-tag
  "ag" 'projectile-ag
  "t" 'projectile-find-file
  "b" 'ido-switch-buffer
  "cc" 'evilnc-comment-or-uncomment-lines
  "," 'switch-to-previous-buffer
  "w"  'kill-buffer
  "nn" 'neotree-toggle
  "nf" 'neotree-find
  "gk" 'windmove-up
  "gj" 'windmove-down
  "gl" 'windmove-right
  "gh" 'windmove-left
  "vs" 'split-window-right
  "hs" 'split-window-below
  "x" 'smex)

(global-evil-surround-mode t)

;; autocomplete
(require 'auto-complete)
(global-auto-complete-mode t)


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
(load-theme 'ujelly t)

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
                          (face3 (if active 'powerline-active3 'powerline-inactive3))
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
                                     (powerline-raw " " mode-line)
                                     (funcall separator-left mode-line face1)
                                     (powerline-raw " " face1)
                                     (powerline-major-mode face1)
                                     (powerline-process face1)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-raw " " face2)
                                     (when (buffer-modified-p)
                                       (powerline-raw "+" face2))
                                     (when buffer-read-only
                                       (powerline-raw "RO" face2))
                                     (powerline-raw "%z" face2)
                                     (powerline-raw " " face2)
                                     (funcall separator-left face2 mode-line)
                                     (powerline-raw " " mode-line)
                                     ;; (powerline-raw (concat "[" (mode-line-eol-desc) "]") mode-line)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-minor-modes mode-line)
                                     (powerline-raw "%n " mode-line)))
                          (rhs (list (powerline-raw "%I")
                                     (powerline-raw global-mode-string mode-line 'r)
                                     (powerline-raw "%l-" mode-line 'l)
                                     (powerline-raw "%c ")
                                     (powerline-raw (replace-regexp-in-string  "%" "%%" (format-mode-line '(-3 "%p"))) mode-line 'r)
                                     (when (and vc-mode buffer-file-name)
                                       (let ((backend (vc-backend buffer-file-name)))
                                         (when backend
                                           (powerline-raw (format " %s " (vc-working-revision buffer-file-name backend)) face3))))
                                     (let ((projectile-project (projectile-project-root)))
                                       (when projectile-project
                                         (powerline-raw (format " %s " projectile-project) face2))))))
                     (concat (powerline-render lhs)
                             (powerline-fill mode-line (powerline-width rhs))
                             (powerline-render rhs)))))))

(set-face-attribute 'powerline-active1 nil :background "#3A419A" :foreground "#ffffff" :box nil)
(set-face-attribute 'powerline-inactive1 nil :box nil)
(set-face-attribute 'powerline-active2 nil :background "#B3327B" :foreground "#ffffff" :box nil)
(set-face-attribute 'powerline-inactive2 nil :box nil)
(copy-face 'powerline-active1 'powerline-active3)
(copy-face 'powerline-inactive1 'powerline-inactive3)
(set-face-attribute 'powerline-active3 nil :background "#DFB43E" :foreground "#000000" :box nil)
(set-face-attribute 'powerline-inactive3 nil :box nil)
(set-face-attribute 'mode-line nil :background "#1a1a1a" :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-attribute 'mode-line-highlight nil)
(setq powerline-default-separator nil)
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

;; disable backup files
(setq make-backup-files nil)

;; save desktop by default
;; (desktop-save-mode 1)

;; smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; flx
(setq ido-decorations (quote ("\n↪ "     "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

;; enable ERC irc client
(require 'erc)

(add-to-list 'erc-mode-hook (lambda ()
                              (set (make-local-variable 'scroll-conservatively) 101)
                              (set (make-local-variable 'scroll-margin) 0)))

(setq erc-nick "adlpz")

(setq circe-network-options
      `(("Freenode"
         :nick "adlpz"
         :channels ("#emacs")
         :nickserv-password ,freenode-password
         )))

;; colorize nicks
(require 'erc-hl-nicks)

;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; smartparens
(require 'smartparens-config)

(define-key sp-keymap (kbd "C-l") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-j") 'sp-backward-sexp)
(define-key sp-keymap (kbd "C-k") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-j") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-s-k") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-s-h") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-M-s-h") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-s-l") 'sp-backward-barf-sexp)
(define-key sp-keymap (kbd "C-<backspace>") 'sp-unwrap-sexp)


;; evil-smartparens
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

;; recognise cljs files as clojure
(setq auto-mode-alist (cons '("\\.cljs" . clojure-mode) auto-mode-alist))

(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'highlight-parentheses-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)

;; lisp
(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook #'highlight-parentheses-mode)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

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
    ("0f6e58d1814b4138c5a88241f96547d35883cbb3df6cf9ec8ef44856ece04c13" "ed5af4af1d148dc4e0e79e4215c85e7ed21488d63303ddde27880ea91112b07e" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
