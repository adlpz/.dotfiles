(load-file "~/.emacs.d/init/packages.el")
(load-file "~/.private.el")
(load-file "~/.emacs.d/libs/textwriter-mode.el")

;; textwriter
(require 'textwriter-mode)

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
(require 'helm-config)
(helm-mode 1)
(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)

;; projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; multi web mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                                    (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                                                      (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html"))
(multi-web-global-mode 1)

;; multi-term
(require 'multi-term)
(setq multi-term-program "/usr/bin/zsh")

;; evil
(require 'evil)
(evil-mode 1)
(setq evil-move-cursor-back nil) ;; do not move back on ESC
(setq evil-esc-delay 0)
(setq evil-visual-char 'exclusive)

(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "." 'find-tag
  "ag" 'projectile-ag
  "f" 'helm-find-files
  "s" 'helm-projectile-switch-project
  "b" 'helm-buffers-list
  "p" 'helm-projectile
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
  "x" 'smex
  "t" 'multi-term)

(global-evil-surround-mode t)

;; esc quits everything (lifted from https://github.com/davvil/.emacs.d/blob/master/init.el)
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

;; autocomplete
(require 'company)
(global-company-mode)

;; ui changes
(setq ring-bell-function 'ignore) ;; disable bell
(tool-bar-mode -1) ;; disable toolbar
(menu-bar-mode -1) ;; disable menu bar
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
(load-theme 'base16-tomorrow-dark t)

;; override terminal fonts, as the definition changed

;; term
(defface term-color-black 
           '((t (:foreground "#3f3f3f" :background "#272822"))) 
             "Unhelpful docstring.")
(defface term-color-red
           '((t (:foreground "#cc9393" :background "#272822"))) 
             "Unhelpful docstring.")
(defface term-color-green
           '((t (:foreground "#7f9f7f" :background "#272822"))) 
             "Unhelpful docstring.")
(defface term-color-yellow
           '((t (:foreground "#f0dfaf" :background "#272822"))) 
             "Unhelpful docstring.")
(defface term-color-blue 
           '((t (:foreground "#6d85ba" :background "#272822"))) 
             "Unhelpful docstring.")
(defface term-color-magenta 
           '((t (:foreground "#dc8cc3" :background "#272822"))) 
             "Unhelpful docstring.")
(defface term-color-cyan
           '((t (:foreground "#93e0e3" :background "#272822"))) 
             "Unhelpful docstring.")
(defface term-color-white
           '((t (:foreground "#dcdccc" :background "#272822"))) 
             "Unhelpful docstring.")
'(term-default-fg-color ((t (:inherit term-color-white))))
'(term-default-bg-color ((t (:inherit term-color-black))))

(setq ansi-term-color-vector
        [term term-color-black term-color-red term-color-green term-color-yellow 
                  term-color-blue term-color-magenta term-color-cyan term-color-white])

;; highlight current line
(global-hl-line-mode 1)

;; font
;;(set-face-attribute 'default t :font "Consolas-13")

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
(add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;; smartparens
(require 'smartparens-config)
(define-key sp-keymap (kbd "C-S-l") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-S-h") 'sp-forward-barf-sexp) 
(define-key sp-keymap (kbd "C-M-S-h") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-S-l") 'sp-backward-barf-sexp)
(define-key sp-keymap (kbd "C-l") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-h") 'sp-backward-sexp)
(define-key sp-keymap (kbd "C-k") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-j") 'sp-down-sexp)
;;(define-key sp-keymap (kbd "C-s-<268632076>") 'sp-forward-slurp-sexp) ;; h
;;(define-key sp-keymap (kbd "C-s-<268632072>") 'sp-forward-barf-sexp) ;; j
;;(define-key sp-keymap (kbd "C-M-s-<268632072>") 'sp-backward-slurp-sexp) ;; h
;;(define-key sp-keymap (kbd "C-M-s-<268632076>") 'sp-backward-barf-sexp) ;; j
(define-key sp-keymap (kbd "C-<backspace>") 'sp-unwrap-sexp)

;; evil-smartparens
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

;; recognise cljs files as clojure
(setq auto-mode-alist (cons '("\\.cljs" . clojure-mode) auto-mode-alist))

;; clojure
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'highlight-parentheses-mode)

(require 'clj-refactor)

(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import
    (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;; lisp
(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook #'highlight-parentheses-mode)

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

;;email
(require 'mu4e)
(setq mu4e-maildir (expand-file-name "~/Mail"))
(setq
 user-mail-address "a@rtf.cc"
 user-full-name "Adrià López"
 )
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials
      '(("m.rtf.cc" 587 nil nil))
      smtp-auth-credentials
      (expand-file-name "~/.authinfo.gpg")
      smtpmail-default-smtp-server "m.rtf.cc"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

;; backups and autosave
(setq
 backup-directory-alist `(("." . "~/.saves"))
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 auto-save-default t
 auto-save-timeout 20
 auto-save-interval 200)

;; -- org mode -- 

;; Italics
(setq org-hide-emphasis-markers t)

;; Better bullet points
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
;; Header Bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Better headers
(let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                             ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                             ((x-list-fonts "Verdana")         '(:font "Verdana"))
                             ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.2))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.4))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.4 :underline nil))))))

;; custom variables

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
