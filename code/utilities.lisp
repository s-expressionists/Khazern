;;;; Copyright (c) 2008, 2009, 2010, 2011, 2012, 2013, 2014
;;;;
;;;;     Robert Strandh (robert.strandh@gmail.com)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

(cl:in-package #:sicl-loop)

;;; Loop keywords are symbols, but they are not recognized by symbol
;;; identity as is usually the case, but instead by their names.  The
;;; HyperSpec doesn't say what function to use for comparing the
;;; names.  We assume string= here, meaning that the names are case 
;;; sensitive. 

(defun symbol-equal (symbol1 symbol2)
  (and (symbolp symbol1)
       (string= symbol1 symbol2)))

;;; This function generates code for destructuring a value according
;;; to a tree of variables.  D-VAR-SPEC is a tree of variable names
;;; (symbols).  FORM is a form that, at runtime, computes the value to
;;; be assigned to the root of D-VAR-SPEC.  This function returns a
;;; list of bindings to be used in a LET* form.  These bindings
;;; destructure the root value until the leaves of the tree are
;;; reached, generating intermediate temporary variables as necessary.
;;; The destructuring code calls the function LIST-CAR and LIST-CDR so
;;; that an error is signaled whenever the corresponding place in the
;;; value tree is not a CONS cell.
(defun destructure-variables (d-var-spec form)
  (let ((bindings '()))
    (labels ((traverse (d-var-spec form)
	       (cond ((null d-var-spec)
		      nil)
		     ((symbolp d-var-spec)
		      (push `(,d-var-spec ,form) bindings))
		     ((not (consp d-var-spec))
		      (error 'expected-var-spec-but-found
			     :found d-var-spec))
		     (t
		      (let ((temp (gensym)))
			(push `(,temp ,form) bindings)
			(traverse (car d-var-spec) `(list-car ,temp))
			(traverse (cdr d-var-spec) `(list-cdr ,temp)))))))
      (traverse d-var-spec form)
      (reverse bindings))))
