;;; Search and view Clojure documentation from clojuredocs.org
;;;

(require 'clojure-mode-ext)
(require 'clojure-slime-ext)

(defconst clojuredocs-uri-prefix "http://clojuredocs.org")
(defconst google-uri-prefix "http://google.com")

(defun clojuredocs-find-uri (sym &optional prefix-map uri-formatter)
  (let ((match (find-if (lambda (pair)
			  (string-match (first pair) sym))
			(or prefix-map clojuredocs-prefix-map))))
    (when match
      (funcall (or uri-formatter 'clojuredocs-uri-formatter) sym (cdr match)))))

(defvar clojuredocs-prefix-map
  '(("^ring" . "/ring/%s")
    ("^clojure\.core" . "/clojure_core/%s")))

(defun clojuredocs-uri-formatter (sym format-string)
  (concat clojuredocs-uri-prefix (format format-string sym) "#top"))

(defvar javadocs-prefix-map
  '(("^\\(java[x]?.\\|org.ietf.\\|org.omg.\\|org.w3c.\\|org.xml.\\)"
     . "http://docs.oracle.com/javase/6/docs/api/%s.html")
    ("^org\.eclipse.jetty"
     . "http://download.eclipse.org/jetty/stable-7/apidocs/%s.html")
    ("^org\.apache\.log4j"
     . "http://logging.apache.org/log4j/1.2/apidocs/%s.html")
    ("^org\.apache\.commons\.codec"
     . "http://commons.apache.org/codec/apidocs/%s.html")))

(defun javadocs-uri-formatter (sym format-string)
  (format format-string (replace-regexp-in-string "\\." "/" sym)))

(defun clojuredocs-render (sap type)
  (destructuring-bind (sap type)
      values
    (cond
     ((eq :var type)
      (browse-url
       (or (clojuredocs-find-uri sap)
           (format "%s/search?q=%s"
                   clojuredocs-uri-prefix
                   (first (last (split-string sap "/")))
                   (replace-regexp-in-string "[ ]+" "+" sap)))))
     ((eq :class type)
      (browse-url
       (or (clojuredocs-find-uri sap javadocs-prefix-map 'javadocs-uri-formatter)
           (format "%s/search?%s&q=%s+javadoc"
                   google-uri-prefix
                   "btnI=I%27m+Feeling+Lucky&ie=UTF-8&oe=UTF-8"
                   sap)))))))

(defun clojuredocs ()
  "Get the symbol at point; or prompt for a symbol name, look it
up in the clojuredocs.org, or find a javadoc (if a Java class)
and render the results using the default web browser."
  (interactive)
  (if (not (slime-connected-p))
      (message "No clojure to talk to.")
    (destructuring-bind (status &rest values)
	(cljx/slime-resolve-symbol (or (cljx/symbol-at-point)
				       (region)
				       (read-string "ClojureDocs: ")))
      (if (eq :error status)
	  (apply 'message values)
	(apply 'clojuredocs-render values)))))

(provide 'clojuredocs)
