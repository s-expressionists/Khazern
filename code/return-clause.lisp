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

(defclass return-clause (unconditional-clause form-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser return-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form)
                 (make-instance 'return-clause
                                :form form))
               (keyword :return)
               'terminal
               'anything))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute body-forms.

(defmethod body-forms ((clause return-clause))
  (let ((form (form clause)))
    `((return-from ,*loop-name*
        ,(if (and *it-var* (it-keyword-p form))
             *it-var*
             form)))))
