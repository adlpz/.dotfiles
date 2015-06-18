
(require 'visual-fill-column)

(defun textwriter-mode--enable ()
  "Enable the textwriter mode"
  ;; Save visual-fill-column-mode
  (setq textwriter-mode--saved-visual-fill-column-mode visual-fill-column-mode)
  ;; Set variables for visual-fill-mode
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t
        visual-fill-column-fringes-outside-margins t)
  (visual-fill-column-mode 1)

  ;; Set face mode
  (textwriter-set-font))

(defun textwriter-mode--disable ()
  "Disable the textwriter mode"
  ;; Disable visual-fill-mode
  (kill-local-variable 'visual-fill-column-width)
  (kill-local-variable 'visual-fill-column-center-text)
  (visual-fill-column-mode -1)

  ;; Disable face mode
  (buffer-face-mode -1))

(defun textwriter-set-font ()
  "Set font to a nice text writing font"
  (interactive)
  (setq buffer-face-mode-face '(:family "Source Sans Pro" :height 140))
  (buffer-face-mode 1))

(define-minor-mode textwriter-mode
  "Minor mode to write text nicely"
  :init-value nil :lighter "txtwrtr" :global nil
  (if textwriter-mode
      (textwriter-mode--enable)
    (textwriter-mode--disable)))

(defun turn-on-textwriter-mode ()
  "Required by define-globalized-minor-mode"
  (textwriter-mode 1))

(define-globalized-minor-mode
  global-textwriter-mode
  textwriter-mode
  turn-on-textwriter-mode
  :require 'textwriter-mode)

(provide 'textwriter-mode)
