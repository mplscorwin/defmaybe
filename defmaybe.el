;;; defmaybe.el --- maybe declare variables
;;; AUTHOR: Corwin Brust <corwin@bru.st>
;;; LICENSE: GPL2 or newer GNU Public License
;;; VERSION: 0.2

;;; Commentary:
;;
;;    A drop-in replacement for `defvar' that substities 'setq' during "DEVEL".
;;    Place the following lines into a package you are hacking on:
;;      (require 'defmaybe)
;;      (setq defmaybe-defvar t) ;; nil=setq, t=defvar, 'custom=defcustom
;;      (defvar-maybe 'my-var "val" "is documented" :group 'mine :type 'string)
;;
;;    Then use `setvar-maybe' just as you would `setvar'.  To clean things up
;;    (if you don't want provide `defvar-local' along with your work):
;;      defmaybe-chdef('defvar-maybe 'defcustom)
;;
;;    Version 0.2: var is renamed for the pkg and func; add 'custom
;;; TODO: Feature flags, as described elsewhere
;;; TODO: Extract an 'at-point' version of chdef

;;; Dependancies:

(require 'seq)

;;; Code:

(defvar-local defmaybe-defvar nil
  "Local. Controls func: `defvar-maybe', which see for detail.")

(defun defmaybe-changedef (old new &optional max-forms)
  "Set car of `sexp-at-point' to NEW when it is OLD.

If MAX-FORMS is a non-nil, limit forms fowlling NEW to this
number, for exmpla to supress extra arguments required by old but
not accepted by NEW."
  ;;(sexp-at-point
  ;;(read
  (save-excursion
    (let ((count 0)
	  (old-symbol-name (if (stringp old) old
			     (symbol-name old))))
      (goto-char (point-min))
      (while (not (eobp))
	(let* ((orig-sexp (progn (forward-sexp) (sexp-at-point)))
	       (sexp-bds (bounds-of-thing-at-point 'sexp))
	       (sexp-car (and orig-sexp (listp orig-sexp) (car orig-sexp)))
	       (sexp-fun (and sexp-car (symbolp sexp-car) (symbol-name sexp-car))))
	  (when (and sexp-fun (string-equal old-symbol-name sexp-fun))
	    (setq count (1+ count))
	    (delete-region (car sexp-bds) (cdr sexp-bds))
	    (goto-char (car sexp-bds))
	    (insert (with-output-to-string
		      (pp (append (list new)
				  (if (and max-forms
					   (numberp max-forms)
					   (< max-forms (length orig-sexp)))
				      (seq-take (cdr orig-sexp) (abs max-forms))
				    (cdr orig-sexp))))))
	    (delete-char -1))))
      (message "Made %d replacement%s." count (if (not (= count 1)) "s" "")))))

(defmacro defvar-maybe (var-name &optional i-value d-string &rest customize-options)
  "Create or set a variable per the buffer-local `defmaybe-defvar'.

Use `setq' instead of `defvar' when `defmaybe-defvar' is not
truthy.  When `defmaybe-defvar' is `custom use `defcustom'.

VAR-NAME is the symbol to declare or set.  I-VALUE is either
INITVALUE for `defvar' or VAL for `setq'.  D-STRING is DOCSTRING
when `defvar' is used.  CUSTOMIZE-OPTIONS are ARES to `defcustom'.

This is useful when using `eval-buffer' to run test cases hacked
directly into a package's source files."
  (declare (indent 2) (doc-string 3))
;;;  (message "devel:%s (%s)" defmaybe-defvar (and (boundp #'defmaybe-defvar) defmaybe-defvar))
  (if (and (boundp #'defmaybe-defvar)
	   defmaybe-defvar)
      (if (eq 'custom defmaybe-defvar)
	  `(defcustom ,var-name ,i-value ,d-string ,@customize-options)
	`(defvar ,var-name ,i-value ,d-string))
    `(setq ,var-name ,i-value)))

(provide 'defmaybe)
;;; defmaybe.el ends here
