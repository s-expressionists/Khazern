;;;; Copyright (c) 2014
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

(in-package #:sicl-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-EQUALS-THEN.

(defclass for-as-equals-then (for-as-subclause)
  ((%initial-form :initarg :initial-form :reader initial-form)
   (%subsequent-form :initarg :subsequent-form :reader subsequent-form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser for-as-equals-then-parser-1
  (consecutive (lambda (var-spec type-spec = form1 then form2)
		 (declare (ignore = then))
		 (make-instance 'for-as-equals-then
		   :var-spec var-spec
		   :type-spec type-spec
		   :initial-form form1
		   :subsequent-form form2))
	       ;; Accept anything for now.  Analyze later.
	       (singleton #'identity (constantly t))
	       'optional-type-spec-parser
	       (keyword-parser '=)
	       (singleton #'identity (constantly t))
	       (keyword-parser 'then)
	       (singleton #'identity (constantly t))))

(define-parser for-as-equals-then-parser-2
  (consecutive (lambda (var-spec type-spec = form1)
		 (declare (ignore =))
		 (make-instance 'for-as-equals-then
		   :var-spec var-spec
		   :type-spec type-spec
		   :initial-form form1
		   :subsequent-form form1))
	       ;; Accept anything for now.  Analyze later.
	       (singleton #'identity (constantly t))
	       'optional-type-spec-parser
	       (keyword-parser '=)
	       (singleton #'identity (constantly t))))

;;; Make sure parser 1 is tried first.  For that, it must be added
;;; last.
(add-for-as-subclause-parser 'for-as-equals-then-parser-2)
(add-for-as-subclause-parser 'for-as-equals-then-parser-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the bindings.

(defmethod initial-bindings ((clause for-as-equals-then))
  (loop with d-var-spec = (var-spec clause)
	with d-type-spec = (type-spec clause)
	for (variable) in (extract-variables d-var-spec d-type-spec)
	collect `(,variable nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the declarations.

(defmethod declarations ((clause for-as-equals-then))
  (loop with d-var-spec = (var-spec clause)
	with d-type-spec = (type-spec clause)
	for (variable type) in (extract-variables d-var-spec d-type-spec)
	collect `(type (or null ,type) ,variable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue-forms.

(defmethod prologue-forms ((clause for-as-equals-then) end-tag)
  (declare (ignore end-tag))
  (multiple-value-bind (temp-tree dictionary)
      (fresh-variables (var-spec clause))
    `(let* ,(destructure-variables temp-tree (initial-form clause))
       (setq ,@(loop for (original . temp) in dictionary
		     collect original
		     collect temp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step.

(defmethod step ((clause for-as-equals-then))
  (multiple-value-bind (temp-tree dictionary)
      (fresh-variables (var-spec clause))
    `(let* ,(destructure-variables temp-tree (subsequent-form clause))
       (setq ,@(loop for (original . temp) in dictionary
		     collect original
		     collect temp)))))
