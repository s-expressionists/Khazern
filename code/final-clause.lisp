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
  ((%form :initarg :form :reader form)))

;;; The final clause does not bind any variables.
(defmethod bound-variables ((clause final-clause))
  '())

(defmethod accumulation-variables ((clause final-clause))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser final-clause (:body-clause)
  (consecutive (lambda (form)
                 (make-instance 'final-clause :form form))
               (keyword :finally)
               'terminal
               'compound-form+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute epilogue.

(defmethod epilogue-form ((clause final-clause))
  (form clause))
