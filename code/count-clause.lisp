(cl:in-package #:khazern)

(defclass count-clause (count/sum-accumulation-clause) ())

(defclass count-form-clause (count-clause form-mixin)
  ())

(defclass count-it-clause (count-form-clause it-mixin)
  ())

(defclass count-form-into-clause (into-mixin count-clause form-mixin)
  ())

(defclass count-it-into-clause (count-form-into-clause it-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser count-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var type-spec
                        &aux (itp (it-keyword-p form)))
                 (cond ((and itp var)
                        (make-instance 'count-it-into-clause
                                       :form form
                                       :into-var var
                                       :type-spec type-spec))
                       (itp
                        (make-instance 'count-it-clause
                                       :form form
                                       :type-spec type-spec))
                       (var
                        (make-instance 'count-form-into-clause
                                       :form form
                                       :into-var var
                                       :type-spec type-spec))
                       (t
                        (make-instance 'count-form-clause
                                       :form form
                                       :type-spec type-spec))))
               (keyword :count :counting)
               'terminal
               'anything
               'optional-into-phrase
               'optional-type-spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.

(defmethod body-forms ((clause count-form-clause) end-tag)
  (declare (ignore end-tag))
  `((when ,(form clause)
      (setq ,*accumulation-variable*
            (1+ ,*accumulation-variable*)))))

(defmethod body-forms ((clause count-form-into-clause) end-tag)
  (declare (ignore end-tag))
  `((when ,(form clause)
      (setq ,(into-var clause)
            (1+ ,(into-var clause))))))

(defmethod body-forms ((clause count-it-clause) end-tag)
  (declare (ignore end-tag))
  (if *it-var*
      `((when ,*it-var*
          (setq ,*accumulation-variable*
                (1+ ,*accumulation-variable*))))
      (call-next-method)))

(defmethod body-forms ((clause count-it-into-clause) end-tag)
  (declare (ignore end-tag))
  (if *it-var*
      `((when ,*it-var*
          (setq ,(into-var clause)
                (1+ ,(into-var clause)))))
      (call-next-method)))
