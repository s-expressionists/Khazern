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

(define-parser minimize-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var type-spec
                        &aux (itp (it-keyword-p form)))
                 (cond ((and itp var)
                        (make-instance 'minimize-it-into-clause
                                       :form form
                                       :into-var var
                                       :type-spec type-spec))
                       (itp
                        (make-instance 'minimize-it-clause
                                       :form form
                                       :type-spec type-spec))
                       (var
                        (make-instance 'minimize-form-into-clause
                                       :form form
                                       :into-var var
                                       :type-spec type-spec))
                       (t
                        (make-instance 'minimize-form-clause
                                       :form form
                                       :type-spec type-spec))))
               (keyword :minimize :minimizing)
               'terminal
               'anything
               'optional-into-phrase
               'optional-type-spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.

(defmethod body-forms ((clause minimize-form-clause) end-tag)
  (declare (ignore end-tag))
  `((cond ((null ,*accumulation-variable*)
           (setq ,*accumulation-variable* ,(form clause))
           (unless (realp ,*accumulation-variable*)
             (error 'type-error :datum ,*accumulation-variable*
                                :expected-type 'real)))
          (t
           (setq ,*accumulation-variable*
                 (min ,*accumulation-variable* ,(form clause)))))))

(defmethod body-forms ((clause minimize-form-into-clause) end-tag)
  (declare (ignore end-tag))
  `((cond ((null ,(into-var clause))
           (setq ,(into-var clause) ,(form clause))
           (unless (realp ,(into-var clause))
             (error 'type-error :datum ,(into-var clause)
                                :expected-type 'real)))
          (t
           (setq ,(into-var clause)
                 (min ,(into-var clause) ,(form clause)))))))

(defmethod body-forms ((clause minimize-it-clause) end-tag)
  (declare (ignore end-tag))
  (if *it-var*
      `((cond ((null ,*accumulation-variable*)
               (setq ,*accumulation-variable* ,*it-var*)
               (unless (realp ,*accumulation-variable*)
                 (error 'type-error :datum ,*accumulation-variable*
                                    :expected-type 'real)))
              (t
               (setq ,*accumulation-variable*
                     (min ,*accumulation-variable* ,*it-var*)))))
      (call-next-method)))

(defmethod body-forms ((clause minimize-it-into-clause) end-tag)
  (declare (ignore end-tag))
  (if *it-var*
      `((cond ((null ,(into-var clause))
               (setq ,(into-var clause) ,*it-var*)
               (unless (realp ,(into-var clause))
                 (error 'type-error :datum ,(into-var clause)
                                    :expected-type 'real)))
              (t
               (setq ,(into-var clause)
                     (min ,(into-var clause) ,*it-var*)))))
      (call-next-method)))
