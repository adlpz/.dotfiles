(require 'boot.repl)

(swap! boot.repl/*default-dependencies*
    concat '[[refactor-nrepl "2.2.0"]
             [cider/cider-nrepl "0.13.0"]
             [org.clojure/tools.nrepl "0.2.12"]])

(swap! boot.repl/*default-middleware*
    conj 'cider.nrepl/cider-middleware
         'refactor-nrepl.middleware/wrap-refactor)
