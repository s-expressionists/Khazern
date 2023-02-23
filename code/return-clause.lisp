(cl:in-package #:khazern)

;;;; Clause RETURN-CLAUSE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class RETURN-CLAUSE.
;;;
;;; An RETURN clause does not exist as a separate grammar item in
;;; the HyperSpec, but we define it here anyway.  The syntax is:
;;;
;;;    return-clause ::= return {form | it}

(defclass return-clause (unconditional-clause)
  ())

(defmethod accumulation-variables ((clause return-clause))
  '())

(defclass return-form-clause (return-clause)
  ((%form :initarg :form :reader form)))

(defclass return-it-clause (return-form-clause)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser return-it-clause ()
  (consecutive (lambda (return it)
                 (declare (ignore return))
                 (make-instance 'return-it-clause
                   :form it))
               (keyword 'return)
               (keyword 'it)))

(define-parser return-form-clause ()
  (consecutive (lambda (return form)
                 (declare (ignore return))
                 (make-instance 'return-form-clause
                   :form form))
               (keyword 'return)
               'anything))

(define-parser return-clause (:body-clause :selectable-clause)
  (alternative 'return-it-clause
               'return-form-clause))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute body-form.

(defmethod body-form ((clause return-form-clause) end-tag)
  (declare (ignore end-tag))
  `(return-from ,*loop-name* ,(form clause)))

(defmethod body-form ((clause return-it-clause) end-tag)
  (declare (ignore end-tag))
  (if *it-var*
      `(return-from ,*loop-name* ,*it-var*)
      (call-next-method)))
