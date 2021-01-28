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
		"\\(.*?\\):\\([0-9]*?\\): \\(\\(?:\\sw\\)*?\\): \\(.*\\)"
		this-error)
	       (setq filename (match-string 1 this-error)
		     pos (string-to-number (match-string 2 this-error))
		     level (match-string 3 this-error)
		     message (match-string 4 this-error))))
	(with-current-buffer buffer
	  ;; Get the line number and column
	  (save-excursion
	    (goto-char pos)
	    (setq error-line (line-number-at-pos))))
	;; Save error in a flycheck-error object
	(when (string= filename this-buffer-name)
	  (add-to-list 'julia-staticlint-errors
		       (flycheck-error-new
			:buffer buffer
			:checker checker
			:filename filename
			:line error-line
			:message message
			:level 'error)))
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
  :predicate (lambda ()
	       ;; Only run when there is a file associated with the buffer
	       (buffer-file-name)))

;;;###autoload
(defun julia-staticlint-init ()
  "Install the flycheck linter."
  (interactive)
  (add-to-list 'flycheck-checkers 'julia-staticlint))

;;;###autoload
(defun julia-staticlint-activate ()
  "Activate the Julia static checker."
  (interactive)
  (setq julia-staticlint-server-proc-buf
	(get-buffer-create julia-staticlint-server-buf-name))
  (unless julia-staticlint-server-proc
    (setq julia-staticlint-server-proc
	  (make-process
	   :name "julia-staticlint-server"
	   :buffer julia-staticlint-server-proc-buf
	   :stderr julia-staticlint-server-proc-buf
	   :command `("julia" ,julia-staticlint-server-path)
	   :noquery t)))
  )

(provide 'julia-staticlint)

;;; julia-staticlint.el ends here
