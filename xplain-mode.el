;;; xplain-mode.el --- Mode for editing Xplain data definition and
;;; data manipulation files
;;
;; $Revision: #1 $
;;

;; Author:   Berend de Boer <berend@pobox.com>
;; Keywords: Xplain
;; URL:      http://www.pobox.com/~berend/Xplain/xplain-mode.el

;;; Commentary:
;;
;; Syntax hightlighting makes creating Xplain databases even more pleasant.
;;
;; If you use use-package, add this to your Emcas init file:
;;
;; (use-package xplain-mode
;;   :straight nil
;;   :load-path "~/src/emacs/Xplain"
;;   :mode "\\.ddl$")
;;
;; Or else simply:
;;
;;(add-to-list 'auto-mode-alist '("\\.ddl\\'" . xplain-mode))
;;(autoload 'xplain-mode "xplain-mode" "Major mode for Xplain programs" t)

;;; Known bugs:
;;  1. SQL code region not detected nor highlighted

;;; Code:

(defgroup xplain nil
  "Major mode for editing Xplain source in Emacs"
  :group 'languages)

;;
(defvar xplain-load-hook nil
  "*Hook run when the xplain-mode is loaded.")

;;
(defvar xplain-font-lock-keywords
  (let ((xplain-keywords (eval-when-compile
			 (concat "\\b"
				 (regexp-opt '(
"and" "any" "assert" "base"
"cascade" "case" "check" "constant" "count"
"database" "default" "delete"
"echo" "else" "end" "extend" "get"
"if" "init" "input" "insert" "its" "loginname" "max" "min"
"newline" "nil" "not" "null" "of" "or"
"per" "purge" "some" "systemdate"
"then" "type" "total"
"update" "value" "with" "where" ) t) "\\b")))
	(xplain-extra-keywords (eval-when-compile
			       (concat "\\b"
				       (regexp-opt '(
"as" "clustered" "index" "inserted" "optional" "procedure" "recompiled procedure" "trigger procedure" "required" "unique" ) t) "\\b")))
	(xplain-types "\(\\(A\\|B\\|D\\|I\\|M\\|P\\|R\\|T\\|V\\)[0-9,]*\)"))
    (setq xplain-font-lock-keywords
	  (list
     (cons "#.*$" 'font-lock-comment-face)
     (cons "\(.include \\|.use \)" font-lock-reference-face)
     (cons "\`[^\']+\'" font-lock-doc-face)
     ;; multiline strings are very, very slow, so they're not fontified
     (cons "\"\\([^\"]*\\|\"\"\\)*\\(\"\\|$\\)" 'font-lock-string-face)
     (cons "\{[^\}]*\}" 'font-lock-function-name-face)
     (cons xplain-keywords 'font-lock-keyword-face)
     (cons xplain-extra-keywords 'font-lock-keyword-face)
     (cons xplain-types 'font-lock-type-face)
     (cons "\\(true\\|false\\)" 'font-lock-constant-face)
     )))
  "Info for function `font-lock-mode'."
)

;;;###autoload
(defun xplain-mode ()
  "Major mode for editing Xplain data definition/manipulation files."
  (interactive)
  ;;
  (kill-all-local-variables)
  (setq mode-name "Xplain")
  (setq major-mode 'xplain-mode)
  ;;
  (setq comment-start "#")
  (setq comment-start-skip "#+ *")
  ;;
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(xplain-font-lock-keywords))
  (make-local-variable 'font-lock-keywords-case-fold-search)
  (setq font-lock-keywords-case-fold-search t)
  (make-local-variable 'font-lock-beginning-of-syntax-function)
  (setq font-lock-beginning-of-syntax-function 'beginning-of-line)
  ;; why do I have this??
  (make-local-variable 'font-lock-keywords-only)
  (setq font-lock-keywords-only t)
  ;;
  (run-hooks 'xplain-mode-hook)
  )

(provide 'xplain-mode)
(run-hooks 'xplain-load-hook)

;;; xplain-mode.el ends here
