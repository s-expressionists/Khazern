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
  ((%body :initarg :body :reader body)))

(defmethod accumulation-variables ((clause do-clause))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser do-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form)
                 (make-instance 'do-clause :body form))
               (keyword :do :doing)
               'terminal
               'compound-form+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the body-form.

(defmethod body-form ((clause do-clause) end-tag)
  (declare (ignore end-tag))
  (body clause))
