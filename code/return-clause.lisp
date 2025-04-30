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

(defclass return-form-clause (return-clause)
  ((%form :initarg :form :reader form)))

(defclass return-it-clause (return-form-clause)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser return-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form)
                 (make-instance (if (it-keyword-p form)
                                    'return-it-clause
                                    'return-form-clause)
                                :form form))
               (keyword :return)
               'terminal
               'anything))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute body-forms.

(defmethod body-forms ((clause return-form-clause) end-tag)
  (declare (ignore end-tag))
  `((return-from ,*loop-name* ,(form clause))))

(defmethod body-forms ((clause return-it-clause) end-tag)
  (declare (ignore end-tag))
  (if *it-var*
      `((return-from ,*loop-name* ,*it-var*))
      (call-next-method)))
