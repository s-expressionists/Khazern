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

(define-parser maximize-it-into-clause ()
  (consecutive (lambda (maximize it into var type-spec)
                 (declare (ignore maximize into))
                 (make-instance 'maximize-it-into-clause
                   :form it
                   :into-var var
                   :type-spec type-spec))
               (keyword 'maximize 'maximizing)
               (keyword 'it)
               (keyword 'into)
               (singleton #'identity
                          (lambda (x)
                            (and (symbolp x) (not (constantp x)))))
               'optional-type-spec))

(define-parser maximize-it-clause ()
  (consecutive (lambda (maximize it type-spec)
                 (declare (ignore maximize))
                 (make-instance 'maximize-it-clause
                   :form it
                   :type-spec type-spec))
               (keyword 'maximize 'maximizing)
               (keyword 'it)
               'optional-type-spec))

(define-parser maximize-form-into-clause ()
  (consecutive (lambda (maximize form into var type-spec)
                 (declare (ignore maximize into))
                 (make-instance 'maximize-form-into-clause
                   :form form
                   :into-var var
                   :type-spec type-spec))
               (keyword 'maximize 'maximizing)
               'anything
               (keyword 'into)
               (singleton #'identity
                          (lambda (x)
                            (and (symbolp x) (not (constantp x)))))
               'optional-type-spec))

(define-parser maximize-form-clause ()
  (consecutive (lambda (maximize form type-spec)
                 (declare (ignore maximize))
                 (make-instance 'maximize-form-clause
                   :form form
                   :type-spec type-spec))
               (keyword 'maximize 'maximizing)
               'anything
               'optional-type-spec))

(define-parser maximize-clause  (:body-clause :selectable-clause)
  (alternative 'maximize-it-into-clause
               'maximize-it-clause
               'maximize-form-into-clause
               'maximize-form-clause))

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
