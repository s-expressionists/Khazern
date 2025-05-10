(cl:in-package #:khazern)

(defclass sum-clause (count/sum-accumulation-clause form-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser sum-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var type-spec)
                 (make-instance 'sum-clause
                                :form form
                                :into-var var
                                :type-spec type-spec))
               (keyword :sum :summing)
               'terminal
               'anything
               'optional-into-phrase
               'optional-type-spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.

(defmethod body-forms ((clause sum-clause))
  (let ((form (form clause)))
    `((incf ,(into-var clause)
            ,(if (and *it-var* (it-keyword-p form))
                 *it-var*
                 form)))))
