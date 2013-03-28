;;; Emacs clojurescript-mode extensions

(require 'clojure-mode-ext)

(defun cljx/clojurescript-mode-hook ()
  ;; disable slime as clojurescript uses inferior-lisp-mode
  (slime-mode -1))

(require 'clojurescript-mode)
(add-hook 'clojurescript-mode-hook 'cljx/clojurescript-mode-hook)

(provide 'clojurescript-mode-ext)
