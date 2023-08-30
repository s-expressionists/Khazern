(cl:in-package #:khazern)

;;;; Clause DO-CLAUSE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DO-CLAUSE.
;;;
;;; An DO clause does not exist as a separate grammar item in
;;; the HyperSpec, but we define it here anyway.  The syntax is:
;;;
;;;    do-clause ::= do compound-form+

(defclass do-clause (unconditional-clause)
  ((%forms :initarg :forms :reader forms)))

(defmethod accumulation-variables ((clause do-clause))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser do-clause (:body-clause :selectable-clause)
  (consecutive (lambda (forms)
                 (make-instance 'do-clause :forms forms))
               (keyword :do :doing)
               'terminal
               'compound-form+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the body-forms.

(defmethod body-forms ((clause do-clause) end-tag)
  (declare (ignore end-tag))
  (forms clause))
