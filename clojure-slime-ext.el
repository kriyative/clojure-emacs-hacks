;;; extends the swank command set

(defvar *cljx-slime-commands*
  `(do
     (ns swank.commands.cljx)
     (swank.commands/defslimefn cljx-resolve-symbol [ns-str sym-str]
       (if-let [the-ns (find-ns (symbol ns-str))]
         (if-let [resolved (ns-resolve the-ns (symbol sym-str))]
	   (cond
	     (var? resolved) (let [^clojure.lang.Var the-var resolved]
			       (list :ok
                                     (str (.ns the-var) "/" (.sym the-var))
                                     :var))
	     (class? resolved) (let [^java.lang.Class the-class resolved]
				 (list :ok (.getName the-class) :class)))
	   (list :error (format "Can't resolve symbol %s." sym-str)))
	 (list :error
               (format "Can't find namespace %s. Consider compiling buffer."
                       ns-str)))))
  )

(defun cljx/form->string (expr)
  (replace-regexp-in-string "\\\\\\(.\\)" "\\1" (prin1-to-string expr)))

(defun cljx/inject-slime-commands ()
  (slime-eval-async `(interactive-eval
                      ,(cljx/form->string *cljx-slime-commands*))
    (lambda (&rest args) (message "cljx-slime-commands injected ok."))))
     
(add-hook 'slime-connected-hook 'cljx/inject-slime-commands)

(defun symbol-at-point-as-string ()
  (let ((sym (symbol-at-point)))
    (when sym (symbol-name sym))))

(defun cljx/symbol-at-point ()
  (let ((sap (symbol-at-point)))
    (when sap
      (replace-regexp-in-string  "\\.*$"
				 ""
				 (substring-no-properties
				  (symbol-name sap))))))

(defun cljx/slime-resolve-symbol (sap)
  (when (and (slime-connected-p) sap)
    (slime-eval `(cljx-resolve-symbol ,(slime-current-package) ,sap))))

(defun cljx/slime-resolve-symbol-at-point ()
  (cljx/slime-resolve-symbol (cljx/symbol-at-point)))

(provide 'clojure-slime-ext)
