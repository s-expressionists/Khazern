(cl:in-package #:khazern)

(defclass maximize-clause (max/min-accumulation-clause) ())

(defclass maximize-form-clause (maximize-clause form-mixin)
  ())

(defclass maximize-it-clause (maximize-form-clause it-mixin)
  ())

(defclass maximize-form-into-clause (into-mixin maximize-clause form-mixin)
  ())

(defclass maximize-it-into-clause (maximize-form-into-clause it-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser maximize-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var type-spec
                        &aux (itp (it-keyword-p form)))
                 (cond ((and itp var)
                        (make-instance 'maximize-it-into-clause
                                       :form form
                                       :into-var var
                                       :type-spec type-spec))
                       (itp
                        (make-instance 'maximize-it-clause
                                       :form form
                                       :type-spec type-spec))
                       (var
                        (make-instance 'maximize-form-into-clause
                                       :form form
                                       :into-var var
                                       :type-spec type-spec))
                       (t
                        (make-instance 'maximize-form-clause
                                       :form form
                                       :type-spec type-spec))))
               (keyword :maximize :maximizing)
               'terminal
               'anything
               'optional-into-phrase
               'optional-type-spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.

(defmethod body-forms ((clause maximize-form-clause) end-tag)
  (declare (ignore end-tag))
  `((cond ((null ,*accumulation-variable*)
           (setq ,*accumulation-variable* ,(form clause))
           (unless (realp ,*accumulation-variable*)
             (error 'type-error :datum ,*accumulation-variable*
                                :expected-type 'real)))
          (t
           (setq ,*accumulation-variable*
                 (max ,*accumulation-variable* ,(form clause)))))))

(defmethod body-forms ((clause maximize-form-into-clause) end-tag)
  (declare (ignore end-tag))
  `((cond ((null ,(into-var clause))
           (setq ,(into-var clause) ,(form clause))
           (unless (realp ,(into-var clause))
             (error 'type-error :datum ,(into-var clause)
                                :expected-type 'real)))
          (t
           (setq ,(into-var clause)
                 (max ,(into-var clause) ,(form clause)))))))

(defmethod body-forms ((clause maximize-it-clause) end-tag)
  (declare (ignore end-tag))
  (if *it-var*
      `((cond ((null ,*accumulation-variable*)
               (setq ,*accumulation-variable* ,*it-var*)
               (unless (realp ,*accumulation-variable*)
                 (error 'type-error :datum ,*accumulation-variable*
                                    :expected-type 'real)))
              (t
               (setq ,*accumulation-variable*
                     (max ,*accumulation-variable* ,*it-var*)))))
      (call-next-method)))

(defmethod body-forms ((clause maximize-it-into-clause) end-tag)
  (declare (ignore end-tag))
  (if *it-var*
      `((cond ((null ,(into-var clause))
               (setq ,(into-var clause) ,*it-var*)
               (unless (realp ,(into-var clause))
                 (error 'type-error :datum ,(into-var clause)
                                    :expected-type 'real)))
              (t
               (setq ,(into-var clause)
                     (max ,(into-var clause) ,*it-var*)))))
      (call-next-method)))
