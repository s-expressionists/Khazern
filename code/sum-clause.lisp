(cl:in-package #:khazern)

(defclass sum-clause (count/sum-accumulation-clause form-mixin)
  ())

(defclass sum-it-clause (sum-clause it-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser sum-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var type-spec
                        &aux (itp (it-keyword-p form)))
                 (cond ((and itp var)
                        (make-instance 'sum-it-clause
                                       :form form
                                       :into-var var
                                       :type-spec type-spec))
                       (itp
                        (make-instance 'sum-it-clause
                                       :form form
                                       :type-spec type-spec))
                       (var
                        (make-instance 'sum-clause
                                       :form form
                                       :into-var var
                                       :type-spec type-spec))
                       (t
                        (make-instance 'sum-clause
                                       :form form
                                       :type-spec type-spec))))
               (keyword :sum :summing)
               'terminal
               'anything
               'optional-into-phrase
               'optional-type-spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.

(defmethod body-forms ((clause sum-clause) end-tag)
  (declare (ignore end-tag))
  `((incf ,(into-var clause) ,(form clause))))

(defmethod body-forms ((clause sum-it-clause) end-tag)
  (declare (ignore end-tag))
  (if *it-var*
      `((incf ,(into-var clause) ,*it-var*))
      (call-next-method)))
