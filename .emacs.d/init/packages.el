;; packages
(setq package-list
      '(async ;; async stuff, requirement of others
        popup ;; popup for autocompletion and others
        cider ;; clojure interactive tools
        clojure-mode ;; clojure mode
        clj-refactor ;; clojure refactoring tools
        circe ;; irc client
        csv-mode ;; csv mode
        erc-hl-nicks ;; colorful nicks in erc
        evil-leader ;; leader key in evil
        evil ;; vim emulation
        undo-tree ;; undo is a tree
        evil-smartparens ;; smartparens in evil mode
        smartparens ;; smart parentheses
        evil-surround ;; surround from vim in evil
        flx-ido ;; fuzzy something
        helm ;; incremental search framework
        helm-projectile ;; projectile integration for helm
        highlight-parentheses ;; highlight matching parens in lisps
        neotree ;; vim's nerdtree port
        paredit ;; editing sexprs
        evil-paredit ;; paredit in vim mode
        powerline-evil ;; evil mode in powerline
        powerline ;; cool modeline
        projectile ;; project management
        relative-line-numbers ;; line number relative to current
        smex ;; m-x fuzzy search
        ujelly-theme ;; cool theme
        base16-theme ;; nice, lotsa themes http://chriskempson.github.io/base16/
        writeroom-mode ;; whiteroom no-distraction mode
        visual-fill-column ;; wrap on same-width columns
        multiple-cursors ;; multiple cursors!!
        guide-key ;; show next possible key combos to press
        package-utils ;; utils for package management
        company ;; autocomplete magic. Good for CIDER.
        magit ;; git
        lua-mode ;; lua
        ;; disabled: messess up with namespaces: aggressive-indent ;; indents whole blocks and keeps all nice
        ))

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

;; lifted from emacs 25
(defun package--find-non-dependencies ()
  "Return a list of installed packages which are not dependencies.
Finds all packages in `package-alist' which are not dependencies
of any other packages.
Used to populate `package-selected-packages'."
  (let ((dep-list
         (delete-dups
          (apply #'append
            (mapcar (lambda (p) (mapcar #'car (package-desc-reqs (cadr p))))
                    package-alist)))))
    (cl-loop for p in package-alist
             for name = (car p)
             unless (memq name dep-list)
             collect name)))

;; List packages not in the above list
(defun untracked-packages ()
  (let ((filter (lambda (lst condp)
                  (delq nil
                        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))))
    (funcall filter
     (package--find-non-dependencies)
     (lambda (p)
       (not (member p package-list))))))

;; Clean packages installes but not in the above list
(defun clean-untracked-packages ()
  (interactive)
  (dolist (package (untracked-packages))
    (if (y-or-n-p (format "Remove package %s?" package))
      (package-delete (cadr (assq package package-alist))))))
