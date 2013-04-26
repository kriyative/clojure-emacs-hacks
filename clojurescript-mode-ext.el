;;; Emacs clojurescript-mode extensions

(require 'clojure-mode-ext)

(defun cljx/clojurescript-mode-hook ()
  ;; disable slime as clojurescript uses inferior-lisp-mode
  (when (boundp 'slime-mode)
    (slime-mode -1))
  (define-key clojurescript-mode-map "\C-x\C-e" 'clojurescript-eval-last-expression)
  (define-key clojurescript-mode-map "\C-c\C-k" 'clojurescript-compile-and-load-file)
  (define-key clojurescript-mode-map " " 'slime-space)
  (define-key clojurescript-mode-map "\C-c\C-dd" 'slime-describe-symbol))

(require 'clojurescript-mode)
(add-hook 'clojurescript-mode-hook 'cljx/clojurescript-mode-hook)

(defun clojurescript-output-filter (output)
  (let ((sans-prompt (replace-regexp-in-string "^ClojureScript:[^>]*> " "" output)))
    (when (< 0 (length sans-prompt))
      (message "(%d) %s" (length sans-prompt) sans-prompt))
    output))

(defun clojurescript-inferior-lisp-mode-hook ()
  (with-current-buffer (process-buffer (inferior-lisp-proc))
    (add-hook 'comint-preoutput-filter-functions 'clojurescript-output-filter)))

(defun clojurescript-jack-in ()
  (interactive)
  (let ((inferior-lisp-program "lein trampoline cljsbuild repl-listen")
        (inferior-lisp-mode-hook 'clojurescript-inferior-lisp-mode-hook))
    (inferior-lisp inferior-lisp-program)))

(defun clojurescript-eval-last-expression ()
  (interactive)
  (let ((expr (slime-last-expression)))
    (comint-send-string (inferior-lisp-proc) (concat expr "\n"))))

(defun clojurescript-compile-and-load-file ()
  (interactive)
  (comint-send-string (inferior-lisp-proc) (buffer-string)))

(provide 'clojurescript-mode-ext)
