(cl:in-package #:khazern)

(defclass minimize-clause (max/min-accumulation-clause) ())

(defclass minimize-form-clause (minimize-clause form-mixin)
  ())

(defclass minimize-it-clause (minimize-form-clause it-mixin)
  ())

(defclass minimize-form-into-clause (into-mixin minimize-clause form-mixin)
  ())

(defclass minimize-it-into-clause (minimize-form-into-clause it-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser minimize-it-into-clause ()
  (consecutive (lambda (minimize it into var type-spec)
                 (declare (ignore minimize into))
                 (make-instance 'minimize-it-into-clause
                   :form it
                   :into-var var
                   :type-spec type-spec))
               (keyword 'minimize 'minimizing)
               (keyword 'it)
               (keyword 'into)
               (singleton #'identity
                          (lambda (x)
                            (and (symbolp x) (not (constantp x)))))
               'optional-type-spec))

(define-parser minimize-it-clause ()
  (consecutive (lambda (minimize it type-spec)
                 (declare (ignore minimize))
                 (make-instance 'minimize-it-clause
                   :form it
                   :type-spec type-spec))
               (keyword 'minimize 'minimizing)
               (keyword 'it)
               'optional-type-spec))

(define-parser minimize-form-into-clause ()
  (consecutive (lambda (minimize form into var type-spec)
                 (declare (ignore minimize into))
                 (make-instance 'minimize-form-into-clause
                   :form form
                   :into-var var
                   :type-spec type-spec))
               (keyword 'minimize 'minimizing)
               'anything
               (keyword 'into)
               (singleton #'identity
                          (lambda (x)
                            (and (symbolp x) (not (constantp x)))))
               'optional-type-spec))

(define-parser minimize-form-clause ()
  (consecutive (lambda (minimize form type-spec)
                 (declare (ignore minimize))
                 (make-instance 'minimize-form-clause
                   :form form
                   :type-spec type-spec))
               (keyword 'minimize 'minimizing)
               'anything
               'optional-type-spec))

(define-parser minimize-clause (:body-clause :selectable-clause)
  (alternative 'minimize-it-into-clause
               'minimize-it-clause
               'minimize-form-into-clause
               'minimize-form-clause))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.

(defmethod body-form ((clause minimize-form-clause) end-tag)
  (declare (ignore end-tag))
  `(if (null ,*accumulation-variable*)
       (setq ,*accumulation-variable*
             (ensure-real ,(form clause) 'min-argument-must-be-real))
       (setq ,*accumulation-variable*
             (minimize ,*accumulation-variable* ,(form clause)))))

(defmethod body-form ((clause minimize-form-into-clause) end-tag)
  (declare (ignore end-tag))
  `(if (null ,(into-var clause))
       (setq ,(into-var clause)
             (ensure-real ,(form clause) 'min-argument-must-be-real))
       (setq ,(into-var clause)
             (minimize ,(into-var clause) ,(form clause)))))

(defmethod body-form ((clause minimize-it-clause) end-tag)
  (declare (ignore end-tag))
  (if *it-var*
      `(if (null ,*accumulation-variable*)
           (setq ,*accumulation-variable*
                 (ensure-real ,*it-var* 'min-argument-must-be-real))
           (setq ,*accumulation-variable*
                 (minimize ,*accumulation-variable* ,*it-var*)))
      (call-next-method)))

(defmethod body-form ((clause minimize-it-into-clause) end-tag)
  (declare (ignore end-tag))
  (if *it-var*
      `(if (null ,(into-var clause))
           (setq ,(into-var clause)
                 (ensure-real ,*it-var* 'min-argument-must-be-real))
           (setq ,(into-var clause)
                 (minimize ,(into-var clause) ,*it-var*)))
      (call-next-method)))
