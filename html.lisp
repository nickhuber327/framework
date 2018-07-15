
(defun pairs (lst)
  (labels ((f (lst acc)
	      (split lst
		     (if fail
			 (f (cdr tail) (cons (cons head (car tail)) acc))
			 (reverse acc))
		     (reverse acc))))
	  (f lst nil)))

(defun print-tag (name alst closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
	  (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
	alst)
  (princ #\>))

(defmacro tag (name atts &body body)
  `(progn (print-tag ',name
		     (list ,@(mapcar (lambda (x)
				       `(cons ',(car x) ,(cdr x)))
				     (pairs atts)))
		     nil)
	  ,@body
	  (print-tag ',name nil t)))

(defmacro html (&body body)
  (princ "<!DOCTYPE html>")
  `(tag html ()
	,@body))

(defmacro body (&body body)
  `(tag body ()
	,@body))

(defmacro head (&body body)
  `(tag head ()
	,@body))
