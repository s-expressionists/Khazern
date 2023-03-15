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

(defmethod body-form ((clause maximize-form-clause) end-tag)
  (declare (ignore end-tag))
  `(if (null ,*accumulation-variable*)
       (setq ,*accumulation-variable*
             (ensure-real ,(form clause) 'max-argument-must-be-real))
       (setq ,*accumulation-variable*
             (maximize ,*accumulation-variable* ,(form clause)))))

(defmethod body-form ((clause maximize-form-into-clause) end-tag)
  (declare (ignore end-tag))
  `(if (null ,(into-var clause))
       (setq ,(into-var clause)
             (ensure-real ,(form clause) 'max-argument-must-be-real))
       (setq ,(into-var clause)
             (maximize ,(into-var clause) ,(form clause)))))

(defmethod body-form ((clause maximize-it-clause) end-tag)
  (declare (ignore end-tag))
  (if *it-var*
      `(if (null ,*accumulation-variable*)
           (setq ,*accumulation-variable*
                 (ensure-real ,*it-var* 'max-argument-must-be-real))
           (setq ,*accumulation-variable*
                 (maximize ,*accumulation-variable* ,*it-var*)))
      (call-next-method)))

(defmethod body-form ((clause maximize-it-into-clause) end-tag)
  (declare (ignore end-tag))
  (if *it-var*
      `(if (null ,(into-var clause))
           (setq ,(into-var clause)
                 (ensure-real ,*it-var* 'max-argument-must-be-real))
           (setq ,(into-var clause)
                 (maximize ,(into-var clause) ,*it-var*)))
      (call-next-method)))
