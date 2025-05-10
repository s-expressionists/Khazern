(cl:in-package #:khazern)

(defclass count-clause (count/sum-accumulation-clause form-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser count-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var type-spec)
                 (make-instance 'count-clause
                                :form form
                                :into-var var
                                :type-spec type-spec))
               (keyword :count :counting)
               'terminal
               'anything
               'optional-into-phrase
               'optional-type-spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.

(defmethod body-forms ((clause count-clause))
  (let ((form (form clause))
        (into-var (into-var clause)))
    `((when ,(if (and *it-var* (it-keyword-p form))
                 *it-var*
                 form)
        (setq ,into-var
              (1+ ,into-var))))))
