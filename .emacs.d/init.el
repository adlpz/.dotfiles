(load-file "~/.emacs.d/init/packages.el")
(load-file "~/.private.el")
(load-file "~/.emacs.d/libs/textwriter-mode.el")

(require 'textwriter-mode)

;; projectile
(projectile-global-mode)

;; evil
(require 'evil)
;;(evil-mode 1)
(setq evil-move-cursor-back nil) ;; do not move back on ESC
(setq evil-esc-delay 0)
(setq evil-visual-char 'exclusive)

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

;; remember position on reopen file(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;; powerline
(load-file "~/.emacs.d/init/powerline.el")

;; flx
(setq ido-decorations (quote ("\n↪ "     "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

;; circe IRC
(setq circe-network-options
      `(("Freenode"
         :nick "adlpz"
         :channels ("#emacs")
         :nickserv-password ,freenode-password
         )))

;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; smartparens
(require 'smartparens-config)
(define-key sp-keymap (kbd "C-l") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-h") 'sp-backward-sexp)
(define-key sp-keymap (kbd "C-k") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-j") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-s-<268632076>") 'sp-forward-slurp-sexp) ;; h
(define-key sp-keymap (kbd "C-s-<268632072>") 'sp-forward-barf-sexp) ;; j
(define-key sp-keymap (kbd "C-M-s-<268632072>") 'sp-backward-slurp-sexp) ;; h
(define-key sp-keymap (kbd "C-M-s-<268632076>") 'sp-backward-barf-sexp) ;; j
(define-key sp-keymap (kbd "C-<backspace>") 'sp-unwrap-sexp)

;; evil-smartparens
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

;; recognise cljs files as clojure
(setq auto-mode-alist (cons '("\\.cljs" . clojure-mode) auto-mode-alist))

;; clojure
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'highlight-parentheses-mode)

;; lisp
(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook #'highlight-parentheses-mode)

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
                                        ;(require 'helm-config)
                                        ;(helm-mode 1)
;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-M-]") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-[") 'mc/mark-previous-like-this)
(setq mc/cmds-to-run-for-all
      '(
        evil-append-line
        evil-backward-WORD-begin
        evil-backward-word-begin
        evil-backward-char
        evil-delete-char
        evil-delete-line
        evil-digit-argument-or-evil-beginning-of-line
        evil-emacs-state
        evil-end-of-line
        evil-force-normal-state
        evil-forward-WORD-begin
        evil-forward-WORD-end
        evil-forward-word-begin
        evil-forward-word-end
        evil-forward-char
        evil-insert
        evil-next-line
        evil-normal-state
        evil-previous-line
        evil-exit-visual-state
        ))

;; guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence t)
(guide-key-mode 1)

;; writeroom-mode
(setq writeroom-mode-line t)
(setq writeroom-global-effects '())

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