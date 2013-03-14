(defun clojure-test-load-reporting ()
  "Redefine the test-is report function to store results in metadata."
  (when (eq (compare-strings "clojure" 0 7 (slime-connection-name) 0 7) t)
    (clojure-test-eval-sync
     "(ns clojure.test.mode
        (:use [clojure.test :only [file-position *testing-vars* *test-out*
                                   join-fixtures *report-counters* do-report
                                   test-var *initial-report-counters*]]))

    (def #^{:dynamic true} *clojure-test-mode-out* nil)
    (defn report [event]
     (if-let [current-test (last clojure.test/*testing-vars*)]
        (alter-meta! current-test
                     assoc :status (conj (:status (meta current-test))
                                     [(:type event) (:message event)
                                      (str (:expected event))
                                      (str (:actual event))
                                      (if (and (= (:major *clojure-version*) 1)
                                               (< (:minor *clojure-version*) 2))
                                          ((file-position 2) 1)
                                          (if (= (:type event) :error)
                                              ((file-position 3) 1)
                                              (:line event)))])))
     (binding [*test-out* (or *clojure-test-mode-out* *out*)]
       ((.getRawRoot #'clojure.test/report) event)))

   (defn clojure-test-mode-test-one-var [test-ns test-name]
     (let [v (ns-resolve test-ns test-name)
           once-fixture-fn (join-fixtures (::once-fixtures (meta (find-ns test-ns))))
           each-fixture-fn (join-fixtures (::each-fixtures (meta (find-ns test-ns))))]
       (once-fixture-fn
        (fn []
          (when (:test (meta v))
            (each-fixture-fn (fn [] (test-var v))))))))

    ;; adapted from test-ns
    (defn clojure-test-mode-test-one-in-ns [ns test-name]
      (binding [*report-counters* (ref *initial-report-counters*)]
        (let [ns-obj (the-ns ns)]
          (do-report {:type :begin-test-ns, :ns ns-obj})
          ;; If the namespace has a test-ns-hook function, call that:
          (if-let [v (find-var (symbol (str (ns-name ns-obj)) \"test-ns-hook\"))]
            ((var-get v))
            ;; Otherwise, just test every var in the namespace.
            (clojure-test-mode-test-one-var ns test-name))
          (do-report {:type :end-test-ns, :ns ns-obj}))
        (do-report (assoc @*report-counters* :type :summary))))")))

(defun clojure-test-eval (string &optional handler)
  (slime-eval-async `(swank:eval-and-grab-output ,string)
    (or handler #'identity)))

(defun clojure-test-eval-sync (string)
  (slime-eval `(swank:eval-and-grab-output ,string)))

(defun clojure-test-clear (&optional callback)
  "Remove overlays and clear stored results."
  (interactive)
  (remove-overlays)
  (setq clojure-test-count 0
        clojure-test-failure-count 0
        clojure-test-error-count 0)
  (clojure-test-eval
   "(doseq [t (vals (ns-interns *ns*))]
      (alter-meta! t assoc :status [])
      (alter-meta! t assoc :test nil))"
   callback))

(defun clojure-test-run-test ()
  "Run the test at point."
  (interactive)
  (save-some-buffers nil (lambda () (equal major-mode 'clojure-mode)))
  (clojure-test-clear
   (lambda (&rest args)
     (let* ((f (which-function))
            (test-name (if (listp f) (first f) f)))
       (slime-eval-async
           `(swank:interactive-eval
             ,(format "(binding [clojure.test/report clojure.test.mode/report]
                        (load-file \"%s\")
                        (clojure.test.mode/clojure-test-mode-test-one-in-ns '%s '%s)
                        (cons (:name (meta (var %s))) (:status (meta (var %s)))))"
                      (buffer-file-name)
                      (slime-current-package) test-name
                      test-name test-name))
         (lambda (result-str)
           (let ((result (read result-str)))
             (if (cdr result)
                 (clojure-test-extract-result result)
                 (message "Not in a test.")))))))))

(defun clojure-test-run-tests ()
  "Run all the tests in the current namespace."
  (interactive)
  (save-some-buffers nil (lambda () (equal major-mode 'clojure-mode)))
  (message "Testing...")
  (save-window-excursion
    (if (not (clojure-in-tests-p))
        (clojure-jump-to-test))
    (clojure-test-clear
     (lambda (&rest args)
       ;; clojure-test-eval will wrap in with-out-str
       (slime-eval-async `(swank:load-file
                           ,(slime-to-lisp-filename
                             (expand-file-name (buffer-file-name))))
         (lambda (&rest args)
           (slime-eval-async '(swank:interactive-eval
                               "(binding [clojure.test/report
                                               clojure.test.mode/report]
                                                (clojure.test/run-tests))")
             #'clojure-test-get-results)))))))

(eval-after-load 'clojure-test-mode
  '(add-hook 'slime-connected-hook 'clojure-test-load-reporting))

(defun clojure-test-get-results (result)
  (clojure-test-eval
   (concat "(map #(cons (str (:name (meta %)))
                (:status (meta %))) (vals (ns-interns '"
           (slime-current-package) ")))")
   #'clojure-test-extract-results))

(defun clojure-test-extract-results (results)
  (let ((result-vars (read (cadr results))))
    ;; slime-eval-async hands us a cons with a useless car
    (mapc #'clojure-test-extract-result result-vars)
    (clojure-test-echo-results)))

(provide 'clojure-test-mode-slime)
