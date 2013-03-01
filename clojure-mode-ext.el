;;; Emacs clojure-mode extensions 

;;; This file assumes that the clojure-mode, and slime packages are in
;;; the load-path, either added manually or via the packages
;;; system. For more info see:
;;;
;;; https://github.com/technomancy/clojure-mode/blob/master/README.md
;;;

;;; Basic initialization

(defun cljx/customize-indent-settings (indent-function settings)
  "Apply the SETTINGS a-list as overrides to the specified
INDENT-FUNCTION. e.g.,

  (cljx/customize-indent-settings 'clojure-indent-function '((are . let)))

will format `are` forms inside of a `deftest` similar to `let`.
"
  (dolist (x settings)
    (put (car x)
	 indent-function
         (if (numberp (cdr x))
             (cdr x)
           (get (cdr x) indent-function)))))

(defun cljx/basic-init ()
  (setq-default indent-tabs-mode nil    ; no tabs, only spaces

                ;; Ensure all files end with a newline, works around github's
                ;; complaint about "No newline at end"
                require-final-newline t))

(require 'cc-mode)                      ; load java-mode

;;; :::::::::::::::: SLIME setup

(require 'slime)

;;; Stop SLIME from pestering about version mismatches
(setq slime-protocol-version 'ignore)

(defcustom cljx/enable-slime-compile-hacks t
  "Enable clear/reload behavior when recompiling Clojure
buffers."
  :type 'boolean
  :group 'cljx-settings)

(defun cljx/slime-compile-file (&optional arg)
  "Replacement for the default slime-compile-and-load-file, which
accepts an optional prefix ARG, which when specified clears the
current Clojure namespace using (remove-ns ...) and forces a
recompile.

Keybinding to force a clear and recompile: C-u C-c C-k"
  (interactive "P")
  (let ((package (slime-current-package)))
    (if (and package arg)
        (slime-eval-async `(swank:interactive-eval
                            ,(format "(remove-ns '%s)" package))
          (lambda (result)
            (slime-compile-and-load-file)))
      (slime-compile-and-load-file))))

(defvar *lisp-symbol-to-java-charmap* '(("-" . "_") ("/" . ".")))
(progn
  (defvar *lisp-symbol-to-java-charmap-re*  nil)
  (setq *lisp-symbol-to-java-charmap-re*
        (concat "[" (mapconcat 'car *lisp-symbol-to-java-charmap* "") "]")))

(defun cljx/lisp-symbol-to-java (symbol)
  (replace-regexp-in-string
   *lisp-symbol-to-java-charmap-re*
   (lambda (m) (cdr (assoc m *lisp-symbol-to-java-charmap*)))
   symbol))

(defun cljx/slime-edit-definition (name &optional where)
  "Replacement for default slime-edit-definition, which falls
back to find-tag if slime-edit-definition fails to find
anything (for example, a definition in a Java class)."
  (interactive (list (slime-read-symbol-name "Edit Definition of: ")))
  (let (r)
    (unwind-protect
        (setq r (slime-edit-definition name where))
      (unless r
        (let ((java-name (cljx/lisp-symbol-to-java name)))
          (find-tag java-name))))))

;;; :::::::::::::::: Clojure mode setup

(require 'clojure-mode)
(require 'clojure-test-mode)
(require 'eldoc)

(defun cljx/customize-font-lock ()
  ;; The following is a template for setting up custom font-lock
  ;; (syntax highlight) 
  (setq clojure-font-lock-keywords (append
                                    '(("\\(defn-cond\\)[ \r\n\t]*\\(\\sw+\\)?"
                                       (1 font-lock-keyword-face)
                                       (2 font-lock-function-name-face nil t)))
                                    clojure-font-lock-keywords)))

(defun cljx/slime-mode-hook ()
  (when cljx/enable-slime-compile-hacks
    (define-key slime-mode-map "\C-c\C-k" 'cljx/slime-compile-file))
  (define-key slime-mode-map "\C-\M-i" 'slime-complete-symbol)
  (define-key slime-mode-map "\M-." 'cljx/slime-edit-definition)
  (define-key slime-mode-map "\C-c." 'cljx/slime-edit-definition)
  (define-key slime-mode-map "\C-c^" 'slime-pop-find-definition-stack))

(add-hook 'slime-mode-hook 'cljx/slime-mode-hook)

(defun cljx/clojure-mode-hook ()
  ;; configure special indentation rules for some keywords
  (cljx/customize-indent-settings 'clojure-indent-function '((are . let)))
  (setq clojure-mode-use-backtracking-indent t
        clojure-mode-font-lock-comment-sexp t)
  (slime-mode 1)
  (define-key java-mode-map "\C-c." 'cljx/slime-edit-definition)
  (define-key java-mode-map "\C-c^" 'slime-pop-find-definition-stack)
  (eldoc-mode)
  (clojure-test-mode))

(add-hook 'clojure-mode-hook 'cljx/clojure-mode-hook)

(defun cljx/clojurescript-mode-hook ()
  ;; disable slime as clojurescript uses inferior-lisp-mode
  (slime-mode -1))

(require 'clojurescript-mode)
(add-hook 'clojurescript-mode-hook 'cljx/clojurescript-mode-hook)

;;; :::::::::::::::: Compilation mode setup
(require 'compile)

;;; Help compilation-mode track error locations for use with (C-x `)
(setq compilation-error-regexp-alist
      (append
       (list
	;; works for jikes
	'("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):[0-9]+:[0-9]+:"
	  1 2 3)
	;; works for javac
	'("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):" 1 2)

	;; clojure.test
	'("^FAIL in (.*) (\\([^:]*\\):\\([0-9]+\\)" 1 2)
	'("^ERROR in (.*) (\\([^:]*\\):\\([0-9]+\\)" 1 2))
       compilation-error-regexp-alist))

(provide 'clojure-mode-ext)
