;;; julia-staticlint.el --- Emacs integration for StaticLint.jl -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Danylo Malyuta

;; Version: 1.0
;; Author: Danylo Malyuta <danylo@malyuta.name>
;; Maintainer: Danylo Malyuta <danylo@malyuta.name>
;; URL: https://github.com/dmalyuta/julia-staticlint
;; Keywords: convenience, languages, tools
;; Package-Requires: ((emacs "27.1") (flycheck "32") (julia-mode "0.4"))
;; License: CC0

;; This file is not part of GNU Emacs.

;;; License:

;; To the extent possible under law, Danylo Malyuta has waived all copyright
;; and related or neighboring rights to eglot-jl. This work is published from:
;; United States.

;;; Commentary:

;; This package installs a flycheck static linter for the Julia language, that
;; uses StaticLint.jl (github.com/julia-vscode/StaticLint.jl) as the backend
;; linter. The name of this new linter for flycheck is julia-staticlint. To
;; install the linter, run julia-staticlint-setup. After that, the linter
;; should become active in any julia-mode buffer.

;;; Code:

(require 'flycheck)

(defgroup julia-staticlint nil
  "My customization variables for the init.el file."
  :group 'programming)

(defconst julia-staticlint-server-path
  (expand-file-name "julia_staticlint_server.jl"
		    (file-name-directory load-file-name))
  "Path to the Julia StaticLint.jl background server file.")

(defconst julia-staticlint-client-path
  (expand-file-name "julia_staticlint_client.jl"
		    (file-name-directory load-file-name))
  "Path to the Julia StaticLint.jl client file.")

(defconst julia-staticlint-server-buf-name "*julia-staticlint-server*"
  "Name of the buffer holding the output of the linting server process.")

(defvar julia-staticlint-server-proc-buf nil
  "Server background process buffer.")

(defvar julia-staticlint-server-proc nil
  "Server background process.")

;;;###autoload
(defun julia-staticlint-chars-from-start (pos)
  "Get the number of characters between line start and current
cursor position."
  (save-excursion
    (goto-char pos)
    (forward-line 0)
    (let ((start~col (point)))
      (length (buffer-substring start~col pos)))))

;;;###autoload
(defun parse-julia-staticlint-errors (output checker buffer)
  "Parse the errors for Flycheck, output by StaticLint.jl."
  (setq julia-staticlint-errors nil
	this-buffer-name (buffer-file-name buffer))
  (with-temp-buffer
    (insert output)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((this-error (buffer-substring
			 (point) (progn (forward-line 1) (point)))))
	;; Extract error data using regexps
	(save-match-data
	  (and (string-match
		"\\(.*?\\):\\([0-9]*?\\):\\([0-9]*?\\): \\(\\(?:\\sw\\)*?\\): \\(.*\\)"
		this-error)
	       (setq filename (match-string 1 this-error)
		     pos-beg (string-to-number (match-string 2 this-error))
		     pos-end (string-to-number (match-string 3 this-error))
		     level (match-string 4 this-error)
		     message (match-string 5 this-error))))
	;; Save error in a flycheck-error object
	(when (string= filename this-buffer-name)
	  ;; Check if final line contains a comment starting with `#noerr',
	  ;; which means suppress this error
	  (with-current-buffer buffer
	    (save-excursion
	      (let* ((search-pos-start pos-end)
		     (search-pos-end (progn
				       (goto-char search-pos-start)
				       (forward-line)
				       (1- (point)))))
		(goto-char search-pos-start)
		(setq julia-staticlint-found-ignore
		      (re-search-forward
		       (format "#no%s\\(?:$\\|\s+.*$\\)"
			       (cond ((string= level "error") "err")
				     ((string= level "warn") "warn")
				     ((string= level "info") "info")
				     (t "err"))) search-pos-end t)))))
	  (unless julia-staticlint-found-ignore
	    ;; Extract error location
	    (with-current-buffer buffer
	      (save-excursion
		(goto-char pos-beg)
		(setq line-beg (line-number-at-pos)
		      col-beg (1+ (julia-staticlint-chars-from-start
				   pos-beg)))
		(goto-char pos-end)
		(setq line-end (line-number-at-pos)
		      col-end (1+ (julia-staticlint-chars-from-start
				   pos-end)))))
	    ;; Save the error to list of errors in this buffer
	    (add-to-list 'julia-staticlint-errors
			 (flycheck-error-new
			  :buffer buffer
			  :checker checker
			  :filename filename
			  :message message
			  :level (cond ((string= level "error") 'error)
				       ((string= level "warn") 'warning)
				       ((string= level "info") 'info)
				       (t 'error))
			  ;; Set the error range in text, spanning from
			  ;; (line-beg,col-beg) to (line-end,col-end)
			  :line line-beg
			  :end-line line-end
			  :column col-beg
			  :end-column col-end))))
	)))
  julia-staticlint-errors)

(flycheck-define-checker julia-staticlint
  "A Julia static syntax checker using StaticLint.jl.

See URL `https://github.com/julia-vscode/StaticLint.jl'."
  :command ("julia"
	    (eval (concat julia-staticlint-client-path))
	    source-original)
  :error-parser parse-julia-staticlint-errors
  :modes julia-mode
  :predicate
  (lambda ()
    ;; Only run when there is a file associated with the buffer and
    ;; an existing process is not already running
    (and (file-exists-p (buffer-file-name))
         (not (process-live-p julia-staticlint-server-proc))))
  )

;;;###autoload
(defun julia-staticlint-init ()
  "Install the flycheck linter."
  (interactive)
  (add-to-list 'flycheck-checkers 'julia-staticlint))

;;;###autoload
(defun julia-staticlint-activate ()
  "Activate the Julia static checker."
  (interactive)
  (unless (get-buffer-process julia-staticlint-server-proc-buf)
    ;; Start the background process if it is not already running
    (setq julia-staticlint-server-proc-buf
	  (get-buffer-create julia-staticlint-server-buf-name))
    (setq julia-staticlint-server-proc
	  (make-process
	   :name "julia-staticlint-server"
	   :buffer julia-staticlint-server-proc-buf
	   :stderr julia-staticlint-server-proc-buf
	   :command `("julia" ,julia-staticlint-server-path)
	   :noquery t))))

;;;###autoload
(defun julia-staticlint-stop ()
  "Stop the background Julia static checker process."
  (interactive)
  (when (get-buffer-process julia-staticlint-server-proc-buf)
    (delete-process julia-staticlint-server-proc-buf)))

;;;###autoload
(defun julia-staticlint-restart ()
  "Restart the background Julia static checker process."
  (interactive)
  (julia-staticlint-stop)
  (run-with-timer 1 nil 'julia-staticlint-activate))

(provide 'julia-staticlint)

;;; julia-staticlint.el ends here
