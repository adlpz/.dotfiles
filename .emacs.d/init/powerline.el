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
                                     (let ((projectile-project
                                            (condition-case nil
                                                (projectile-project-root)
                                              (error nil))))
                                       (when projectile-project
                                         (powerline-raw (format " %s " ()projectile-project) face2))))))
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
