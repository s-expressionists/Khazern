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

(define-parser count-it-into-clause ()
  (consecutive (lambda (count it into var type-spec)
                 (declare (ignore count into))
                 (make-instance 'count-it-into-clause
                   :form it
                   :into-var var
                   :type-spec type-spec))
               (keyword 'count 'counting)
               (keyword 'it)
               (keyword 'into)
               (singleton #'identity
                          (lambda (x)
                            (and (symbolp x) (not (constantp x)))))
               'optional-type-spec))

(define-parser count-it-clause ()
  (consecutive (lambda (count it type-spec)
                 (declare (ignore count))
                 (make-instance 'count-it-clause
                   :form it
                   :type-spec type-spec))
               (keyword 'count 'counting)
               (keyword 'it)
               'optional-type-spec))

(define-parser count-form-into-clause ()
  (consecutive (lambda (count form into var type-spec)
                 (declare (ignore count into))
                 (make-instance 'count-form-into-clause
                   :form form
                   :into-var var
                   :type-spec type-spec))
               (keyword 'count 'counting)
               'anything
               (keyword 'into)
               (singleton #'identity
                          (lambda (x)
                            (and (symbolp x) (not (constantp x)))))
               'optional-type-spec))

(define-parser count-form-clause ()
  (consecutive (lambda (count form type-spec)
                 (declare (ignore count))
                 (make-instance 'count-form-clause
                   :form form
                   :type-spec type-spec))
               (keyword 'count 'counting)
               'anything
               'optional-type-spec))

(define-parser count-clause (:body-clause :selectable-clause)
  (alternative 'count-it-into-clause
               'count-it-clause
               'count-form-into-clause
               'count-form-clause))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.

(defmethod body-form ((clause count-form-clause) end-tag)
  (declare (ignore end-tag))
  `(when ,(form clause)
     (setq ,*accumulation-variable*
           (1+ ,*accumulation-variable*))))

(defmethod body-form ((clause count-form-into-clause) end-tag)
  (declare (ignore end-tag))
  `(when ,(form clause)
     (setq ,(into-var clause)
           (1+ ,(into-var clause)))))

(defmethod body-form ((clause count-it-clause) end-tag)
  (declare (ignore end-tag))
  (if *it-var*
      `(when ,*it-var*
         (setq ,*accumulation-variable*
               (1+ ,*accumulation-variable*)))
      (call-next-method)))

(defmethod body-form ((clause count-it-into-clause) end-tag)
  (declare (ignore end-tag))
  (if *it-var*
      `(when ,*it-var*
         (setq ,(into-var clause)
               (1+ ,(into-var clause))))
      (call-next-method)))
 
  
