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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(defmethod parse-tokens
    (client (scope selectable-clauses) (keyword (eql :do)) tokens)
  (make-instance 'do-clause
                 :forms (parse-compound-form+ client scope tokens)))

(defmethod parse-tokens
    (client (scope selectable-clauses) (keyword (eql :doing)) tokens)
  (make-instance 'do-clause
                 :forms (parse-compound-form+ client scope tokens)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the body-forms.

(defmethod body-forms ((clause do-clause))
  (copy-list (forms clause)))
