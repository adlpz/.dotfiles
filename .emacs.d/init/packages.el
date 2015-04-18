;; packages
(setq package-list
      '(auto-complete
        aggressive-indent
        popup
        cider
        queue
        pkg-info
        epl
        dash
        clojure-mode
        circe
        lcs
        lui
        tracking
        shorten
        clojure-mode
        csv-mode
        erc-hl-nicks
        evil-leader
        evil
        goto-chg
        undo-tree
        evil
        goto-chg
        undo-tree
        evil-smartparens
        smartparens
        dash
        evil
        goto-chg
        undo-tree
        evil-surround
        flx-ido
        flx
        goto-last-change
        helm
        async
        highlight-parentheses
        lcs
        lui
        tracking
        shorten
        names
        neotree
        paredit
        popup
        powerline-evil
        powerline
        evil
        goto-chg
        undo-tree
        projectile
        pkg-info
        epl
        dash
        queue
        rainbow-delimiters
        relative-line-numbers
        smartparens
        dash
        smex
        tracking
        shorten
        ujelly-theme
        undo-tree
        writeroom-mode
        visual-fill-column))

;; sources
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ;;                       ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Install all packages in above list
(dolist (package package-list)
  (unless (package-installed-p package)
    (if (y-or-n-p (format "Package %s is missing. Install it? " package))
        (package-install package))))

;; List packages not in the above list
(defun untracked-packages ()
  (let ((filter (lambda (lst condp)
                  (delq nil
                        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))))
    
    (funcall filter
     package-activated-list
     (lambda (p)
       (not (member p package-list))))))

;; Clean packages installes but not in the above list
(defun clean-untracked-packages ()
  (interactive)
  (dolist (package (untracked-packages))
    (if (y-or-n-p (format "Remove package %s?" package))
      (package-delete package))))
