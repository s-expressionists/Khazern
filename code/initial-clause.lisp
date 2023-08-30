(cl:in-package #:khazern)

;;;; Clause INITIAL-CLAUSE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class INITIAL-CLAUSE.
;;;
;;; An INITIAL clause does not exist as a separate grammar item in
;;; the HyperSpec, but we define it here anyway.  The syntax is:
;;;
;;;    initial-clause ::= initially compound-form+

(defclass initial-clause (clause)
  ((%form :initarg :form :reader form)))

;;; The initial clause does not bind any variables.
(defmethod bound-variables ((clause initial-clause))
  '())

(defmethod accumulation-variables ((clause initial-clause))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser initial-clause (:body-clause)
  (consecutive (lambda (form)
                 (make-instance 'initial-clause
                                :form form))
               (keyword :initially)
               'terminal 
               'compound-form+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute prologue-form.

(defmethod prologue-forms ((clause initial-clause) end-tag)
  (declare (ignore end-tag))
  (form clause))
