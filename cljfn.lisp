;;; -*- mode:CommonLisp; package:cljfn -*-

(defpackage #:cljfn
  (:use #:common-lisp)
  (:export #:enable-fn-reader
           #:disable-fn-reader))

(in-package #:cljfn)

(defvar *in-fn-reader* nil)

(defun fn-reader (open-char close-char)
  (lambda (srm char arg)
    (declare (ignore char arg))
    (when *in-fn-reader*
      (error "Nested #~A~As are not allowed" open-char close-char))
    (let ((body (let ((*in-fn-reader* t)) (read-delimited-list close-char srm t)))
	  (arity 0) (rest nil))
      (labels ((walk (expr)
		 (cond ((symbolp expr)
			(let ((name (symbol-name expr)))
			  (cond ((string= name "%")
				 (setq arity (max arity 1)))
				((string= name "%&")
				 (setq rest expr))
				((and (char= (char name 0) #\%)
				      (every #'digit-char-p (subseq name 1)))
				 (setq arity (max arity (parse-integer name :start 1)))))))
		       ((consp expr)
			(walk (car expr))
			(walk (cdr expr))))))
	(walk body))
      (let* ((vars (loop for i from 1 to arity collect (intern (format nil "%~D" i))))
	     (aux (if (> arity 0) `(&aux (,(intern "%") ,(car vars))))))
	`(lambda (,@vars ,@(if rest `(&rest ,rest)) ,@aux)
	   ,body)))))


;; setup & revert *readtable*
(defvar *previous-readtables* nil)

(defun %enable-fn-reader (open-char close-char)
  (push *readtable* *previous-readtables*)
  (setq *readtable* (copy-readtable))
  (set-syntax-from-char close-char #\))
  (set-dispatch-macro-character #\# open-char (fn-reader open-char close-char))
  (values))

(defun %disable-fn-reader ()
  (if *previous-readtables*
      (setq *readtable* (pop *previous-readtables*))
    (setq *readtable* (copy-readtable nil)))
  (values))


;; exports
(defmacro enable-fn-reader (open-char close-char)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%enable-fn-reader ,open-char ,close-char)))

(defmacro disable-fn-reader ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%disable-fn-reader)))
