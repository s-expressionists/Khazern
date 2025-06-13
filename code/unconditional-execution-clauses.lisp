(cl:in-package #:khazern)

(defclass unconditional-clause (selectable-clause)
  ())

;;;; Clause DO-CLAUSE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DO-CLAUSE.
;;;
;;; An DO clause does not exist as a separate grammar item in
;;; the HyperSpec, but we define it here anyway.  The syntax is:
;;;
;;;    do-clause ::= do compound-form+

(defclass do-clause (unconditional-clause compound-forms-mixin)
  ())

(defmethod parse-clause
    (client (scope selectable-superclause) (keyword (eql :do)))
  (make-instance 'do-clause
                 :start *start*
                 :forms (parse-compound-form+)
                 :end *index*))

(defmethod parse-clause
    (client (scope selectable-superclause) (keyword (eql :doing)))
  (make-instance 'do-clause
                 :start *start*
                 :forms (parse-compound-form+)
                 :end *index*))

(defmethod body-forms ((clause do-clause))
  (copy-list (forms clause)))
