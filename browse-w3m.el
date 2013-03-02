;;; Browse urls using emacs-w3m
;;;

(require 'w3m)

(defun w3m-browse-url-other-window (url &optional new-session)
  (save-excursion
    (when (one-window-p)
      (split-window-horizontally))
    (other-window 1)
    (let ((w3m-use-tab nil))
      (w3m-browse-url url new-session))))

(defun w3m-mode-hook ()
  (define-key w3m-mode-map "\M-t" 'w3m-copy-buffer))

(require 'w3m)
(add-hook 'w3m-mode-hook 'w3m-mode-hook)

(setq browse-url-browser-function 'w3m-browse-url-other-window)

(provide 'browse-w3m)
