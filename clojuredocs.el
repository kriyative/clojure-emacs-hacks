;;; Search and view Clojure documentation from clojuredocs.org
;;;

(require 'clojure-mode-ext)
(require 'clojure-slime-ext)
(require 'json)

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

(defun clojuredocs-process-response-json (status
                                          on-success
                                          success-args
                                          on-error
                                          error-args)
  (goto-char (point-min))
  (search-forward-regexp "^HTTP/1.[01] \\([0-9][0-9][0-9]\\)" nil t)
  (unwind-protect
      (if (equal "200" (match-string 1))
          (condition-case c
              (progn
                (message "url-retrieve: OK")
                (search-forward-regexp "^$" nil t)
                (let ((json (json-read)))
                  (apply on-success json success-args)))
            (error
             (message "json-read error: %S" c)
             (apply on-error :json-error error-args)))
        (funcall on-error :url-error))
    (url-mark-buffer-as-dead (current-buffer))))

(defun assoc-val (key alist)
  (let ((a (assoc key alist)))
    (when a
      (cdr a))))

(defun clojuredocs-response-success (json)
  (let ((tmpfilename (make-temp-file "clojuredocs" nil ".html"))
        (title (format "clojuredocs search: %d results" (length json))))
    (message "tmpfilename: %s" tmpfilename)
    (with-temp-file tmpfilename
      (insert
       (format "<html>\n<head><title>%s</title></head>\n" title))
      (insert "<body>\n"
              (format "<h1>%s</h1>\n" title)
              "<ul>\n")
      (loop for el across json
            do (let ((url (assoc-val 'url el))
                     (ns (assoc-val 'ns el))
                     (name (assoc-val 'name el)))
                 (insert
                  (format "<li><a href='%s#top'>%s/%s</a></li>\n"
                          url
                          ns
                          name))))
      (insert "</ul></body></html>\n"))
    (browse-url
     (browse-url-file-url tmpfilename))))

(defun clojuredocs-response-error (error)
  (message "Problem communicating with clojuredocs (%s)" error))

(defun clojuredocs-find-or-search (sap)
  (url-retrieve (format "http://api.clojuredocs.org/search/%s" sap)
                'clojuredocs-process-response-json
                (list (lambda (json sap)
                        (if json
                            (clojuredocs-response-success json)
                          (browse-url
                           (format "%s/search?q=%s+clojure"
                                   google-uri-prefix
                                   sap))))
                      (list sap)
                      'clojuredocs-response-error
                      nil)))

(defun clojuredocs-render (sap type)
  (destructuring-bind (sap type)
      values
    (cond
     ((eq :var type)
      (let ((sap-uri (clojuredocs-find-uri sap)))
        (if sap-uri
            (browse-url sap-uri)
          (clojuredocs-find-or-search sap))))
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
