(cl:in-package #:khazern)

;;;; Clause FINAL-CLAUSE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FINAL-CLAUSE.
;;;
;;; An FINAL clause does not exist as a separate grammar item in
;;; the HyperSpec, but we define it here anyway.  The syntax is:
;;;
;;;    final-clause ::= finally compound-form+

(defclass final-clause (clause)
  ((%forms :initarg :forms :reader forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser final-clause (:body-clause)
  (consecutive (lambda (forms)
                 (make-instance 'final-clause :forms forms))
               (keyword :finally)
               'terminal
               'compound-form+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute epilogue.

(defmethod epilogue-forms ((clause final-clause))
  (copy-list (forms clause)))
