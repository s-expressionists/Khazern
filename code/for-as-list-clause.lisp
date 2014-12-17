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

(defclass for-as-list (for-as-subclause)
  ((%list-form :initarg :list-form :reader list-form)
   (%by-form :initarg :by-form :reader by-form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-IN-LIST.

(defclass for-as-in-list (for-as-list) ())

(define-parser for-as-in-list-parser-1
  (consecutive (lambda (var type-spec in list-form by-form)
		 (declare (ignore in))
		 (make-instance 'for-as-in-list
		   :var-spec var
		   :type-spec type-spec
		   :list-form list-form
		   :by-form by-form))
	       (singleton #'identity (constantly t))
	       'optional-type-spec-parser
	       (keyword-parser 'in)
	       (singleton #'identity (constantly t))
	       'by-parser))

(define-parser for-as-in-list-parser-2
  (consecutive (lambda (var type-spec in list-form)
		 (declare (ignore in))
		 (make-instance 'for-as-in-list
		   :var-spec var
		   :type-spec type-spec
		   :list-form list-form
		   :by-form #'cdr))
	       'd-var-spec-parser
	       'type-spec-parser
	       (keyword-parser 'in)
	       (singleton #'identity (constantly t))))

;;; Define a parser that tries the longer form first
(define-parser for-as-in-list-parser
  (alternative 'for-as-in-list-parser-1
	       'for-as-in-list-parser-2))

(add-for-as-subclause-parser 'for-as-in-list-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-ON-LIST.

(defclass for-as-on-list (for-as-list) ())

(define-parser for-as-on-list-parser-1
  (consecutive (lambda (var type-spec on list-form by-form)
		 (declare (ignore on))
		 (make-instance 'for-as-on-list
		   :var-spec var
		   :type-spec type-spec
		   :list-form list-form
		   :by-form by-form))
	       (singleton #'identity (constantly t))
	       'optional-type-spec-parser
	       (keyword-parser 'on)
	       (singleton #'identity (constantly t))
	       'by-parser))

(define-parser for-as-on-list-parser-2
  (consecutive (lambda (var type-spec on list-form)
		 (declare (ignore on))
		 (make-instance 'for-as-on-list
		   :var-spec var
		   :type-spec type-spec
		   :list-form list-form
		   :by-form #'cdr))
	       'd-var-spec-parser
	       'type-spec-parser
	       (keyword-parser 'on)
	       (singleton #'identity (constantly t))))

;;; Define a parser that tries the longer form first
(define-parser for-as-on-list-parser
  (alternative 'for-as-on-list-parser-1
	       'for-as-on-list-parser-2))

(add-for-as-subclause-parser 'for-as-on-list-parser)
