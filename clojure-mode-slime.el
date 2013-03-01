;;; Clojure-mode integration hooks for SLIME (from pre-2.0
;;; clojure-mode)

;; Copyright © 2007-2011 Jeffrey Chu, Lennart Staflin, Phil Hagelberg
;;
;; Authors: Jeffrey Chu <jochu0@gmail.com>
;;          Lennart Staflin <lenst@lysator.liu.se>
;;          Phil Hagelberg <technomancy@gmail.com>
;; URL: http://github.com/technomancy/clojure-mode

(defvar clojure-project-root-file "project.clj")

;; Pipe to $SHELL to work around mackosecks GUI Emacs $PATH issues.
(defcustom clojure-swank-command
  (if (or (locate-file "lein" exec-path) (locate-file "lein.bat" exec-path))
      "lein jack-in %s"
    "echo \"lein jack-in %s\" | $SHELL -l")
  "The command used to start swank via clojure-jack-in.
For remote swank it is lein must be in your PATH and the remote
proc is launched via sh rather than bash, so it might be necessary
to specific the full path to it. The argument is the port to connect on.
Localhost is assumed."
  :type 'string
  :group 'clojure-mode)

(defcustom clojure-generate-remote-swank-command-function
  'clojure-generate-remote-swank-command-ssh-tunnel
  "A function that is called to determine the swank command that
`clojure-jack-in` will execute and the hostname/port that slime
should connect to for remote projects that are opened via tramp.

The arguments are dir, hostname, and port.  The return value should be an `alist` of the form
(:cmd \"command string\" :hostname \"hostname\" :port 1234)"
  :type 'function
  :group 'clojure-mode)

(defun clojure-generate-local-swank-command-default (dir hostname port)
  (if (not (string-equal "localhost" hostname))
      (error (concat
              "If you need to jack-in to remote projects/jvms over tramp, "
              "you need to define a custom `clojure-generate-swank-command-function`"))
    (list :cmd (format clojure-swank-command port)
          :hostname hostname
          :port port)))

(defun clojure-generate-remote-swank-command-ssh-tunnel (dir hostname port)
  (destructuring-bind (_method user host localname)
      (append (tramp-dissect-file-name dir) nil)
    (list :cmd (format-spec
                "ssh -L %p:localhost:%p -l '%u' '%h' 'cd \'%d\'; lein jack-in \'%p\''"
                `((?p . ,port)
                  (?h . ,host)
                  (?u . ,(or user (getenv "USER")))
                  (?d . ,localname)))
          :hostname "localhost"
          :port port)))

(defun clojure-generate-swank-cmd (dir hostname port)
  (if (file-remote-p dir)
      (if (functionp clojure-generate-remote-swank-command-function)
          (funcall clojure-generate-remote-swank-command-function dir hostname port)
        (error (concat
                "If you need to jack-in to remote projects/jvms over tramp "
                "you need to define a custom `clojure-generate-remote-swank-command-function`")))
    (clojure-generate-local-swank-command-default dir hostname port)))

(defun clojure-jack-in-sentinel (process event)
  (let ((debug-on-error t))
    (error "Could not start swank server: %s"
           (let ((b (process-buffer process)))
             (if (and b (buffer-live-p b))
                 (with-current-buffer b
                   (buffer-substring (point-min) (point-max))))))))

(defun clojure-eval-bootstrap-region (process)
  "Eval only the elisp in between the markers."
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char 0)
      (search-forward ";;; Bootstrapping bundled version of SLIME")
      (let ((begin (point)))
        (when (not (search-forward ";;; Done bootstrapping." nil t))
          ;; fall back to possibly-ambiguous string if above isn't found
          (search-forward "(run-hooks 'slime-load-hook)"))
        (eval-region begin (point))))))

(defun clojure-kill-swank-buffer (swank-buffer-name)
  (when (get-buffer swank-buffer-name)
    (let ((process (get-buffer-process (get-buffer swank-buffer-name))))
      (if process
          (set-process-query-on-exit-flag process nil))
      (kill-buffer swank-buffer-name))))

(defun clojure-generate-swank-connection-name (dir hostname)
  "swank")

(defun clojure-jack-in-start-process (swank-connection-name swank-buffer-name dir hostname)
  ;; The buffer has to be created before the proc if
  ;; `start-file-process-shell-command` is used. It doesn't hurt to do
  ;; it now even if `start-process-shell-command` is used:
  (get-buffer-create swank-buffer-name)

  (let ((port (- 65535 (mod (caddr (current-time)) 4096))))
    (destructuring-bind (&key cmd hostname port)
        (clojure-generate-swank-cmd dir hostname port)
      (lexical-let* ((proc (start-process-shell-command
                            ;; this command runs locally
                            ;; `start-file-process-shell-command` would
                            ;; run remote for tramp buffers
                            swank-connection-name
                            swank-buffer-name
                            cmd))
                     (dir dir)
                     (hostname hostname)
                     (port port)
                     (connect-callback (lambda () (slime-connect hostname port))))
        (set-process-sentinel proc 'clojure-jack-in-sentinel)
        (set-process-query-on-exit-flag proc nil)
        (set-process-filter proc
                            (lambda (process output)
                              (with-current-buffer (process-buffer process)
                                (insert output))
                              (when (string-match "proceed to jack in" output)
                                (clojure-eval-bootstrap-region process)
                                (with-current-buffer
                                    ;; this block is an attempt to avoid
                                    ;; creating duplicate repl windows
                                    (or
                                     (get-buffer "*slime-repl clojure*")
                                     (get-buffer "*slime-repl nil*")
                                     (current-buffer))
                                  (funcall connect-callback)
                                  (when (string-match "slime-repl" (buffer-name))
                                    ;; this is most likely an old repl
                                    ;; buffer that existed prior to the
                                    ;; jack-in call.
                                    (setq default-directory dir)
                                    (goto-char (point-max))))
                                (set-process-sentinel process nil)
                                (set-process-filter process nil))))))))

;;;###autoload
(defun clojure-jack-in ()
  (interactive)
  (setq slime-net-coding-system 'utf-8-unix)
  (let* ((dir default-directory)
         (hostname (if (file-remote-p default-directory)
                       tramp-current-host "localhost"))
         (connection-name (clojure-generate-swank-connection-name dir hostname))
         (swank-buffer-name (format "*%s*" connection-name)))

    (when (and (functionp 'slime-disconnect)
               (slime-current-connection)
               ;; TODO: ask for permission once jack-in supports multiple connections
               ;; (and (interactive-p) (y-or-n-p "Close old connections first? "))
               )
      (slime-disconnect))
    (clojure-kill-swank-buffer swank-buffer-name)
    (clojure-jack-in-start-process connection-name swank-buffer-name dir hostname))
  (message "Starting swank server..."))

(defun clojure-enable-slime ()
  (slime-mode t)
  (set (make-local-variable 'slime-find-buffer-package-function)
       'clojure-find-ns))

;;;###autoload
(defun clojure-enable-slime-on-existing-buffers ()
  (interactive)
  (add-hook 'clojure-mode-hook 'clojure-enable-slime)
  (save-window-excursion
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (eq major-mode 'clojure-mode)
          (clojure-enable-slime))))))

;;; slime filename translation for tramp
(defun clojure-slime-tramp-local-filename (f)
  (if (file-remote-p f)
      (tramp-file-name-localname
       (tramp-dissect-file-name f))
    f))

(defun clojure-slime-tramp-remote-filename (f)
  (if (file-remote-p default-directory)
      (tramp-make-tramp-file-name
       (tramp-file-name-method
        (tramp-dissect-file-name default-directory))
       (tramp-file-name-user
        (tramp-dissect-file-name default-directory))
       (tramp-file-name-host
        (tramp-dissect-file-name default-directory))
       f)
    f))

(defun clojure-slime-remote-file-name-hook ()
  (setq slime-from-lisp-filename-function
        'clojure-slime-tramp-remote-filename)
  (setq slime-to-lisp-filename-function
        'clojure-slime-tramp-local-filename))

(add-hook 'slime-connected-hook 'clojure-slime-remote-file-name-hook)
(add-hook 'slime-connected-hook 'clojure-enable-slime-on-existing-buffers)
(add-hook 'slime-indentation-update-hooks 'put-clojure-indent)
(add-hook 'slime-indentation-update-hooks 'put-clojure-indent)

(provide 'clojure-mode-slime)
