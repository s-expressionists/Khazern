(cl:in-package #:khazern)

(defclass sum-clause (count/sum-accumulation-clause) ())

(defclass sum-form-clause (sum-clause form-mixin)
  ())

(defclass sum-it-clause (sum-form-clause it-mixin)
  ())

(defclass sum-form-into-clause (into-mixin sum-clause form-mixin)
  ())

(defclass sum-it-into-clause (sum-form-into-clause it-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser sum-it-into-clause-parser
  (consecutive (lambda (sum it into var type-spec)
                 (declare (ignore sum into))
                 (make-instance 'sum-it-into-clause
                   :form it
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'sum)
                            (keyword-parser 'summing))
               (keyword-parser 'it)
               (keyword-parser 'into)
               (singleton #'identity
                          (lambda (x)
                            (and (symbolp x) (not (constantp x)))))
               'optional-type-spec-parser))

(define-parser sum-it-clause-parser
  (consecutive (lambda (sum it type-spec)
                 (declare (ignore sum))
                 (make-instance 'sum-it-clause
                   :form it
                   :type-spec type-spec))
               (alternative (keyword-parser 'sum)
                            (keyword-parser 'summing))
               (keyword-parser 'it)
               'optional-type-spec-parser))

(define-parser sum-form-into-clause-parser
  (consecutive (lambda (sum form into var type-spec)
                 (declare (ignore sum into))
                 (make-instance 'sum-form-into-clause
                   :form form
                   :into-var var
                   :type-spec type-spec))
               (alternative (keyword-parser 'sum)
                            (keyword-parser 'summing))
               'anything-parser
               (keyword-parser 'into)
               (singleton #'identity
                          (lambda (x)
                            (and (symbolp x) (not (constantp x)))))
               'optional-type-spec-parser))

(define-parser sum-form-clause-parser
  (consecutive (lambda (sum form type-spec)
                 (declare (ignore sum))
                 (make-instance 'sum-form-clause
                   :form form
                   :type-spec type-spec))
               (alternative (keyword-parser 'sum)
                            (keyword-parser 'summing))
               'anything-parser
               'optional-type-spec-parser))

(define-parser sum-clause-parser
  (alternative 'sum-it-into-clause-parser
               'sum-it-clause-parser
               'sum-form-into-clause-parser
               'sum-form-clause-parser))

(add-clause-parser 'sum-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.

(defmethod body-form ((clause sum-form-clause) end-tag)
  (declare (ignore end-tag))
  `(setq ,*accumulation-variable*
         (sum ,*accumulation-variable* ,(form clause))))

(defmethod body-form ((clause sum-form-into-clause) end-tag)
  (declare (ignore end-tag))
  `(setq ,(into-var clause)
         (sum ,(into-var clause) ,(form clause))))

(defmethod body-form ((clause sum-it-clause) end-tag)
  (declare (ignore end-tag))
  (if *it-var*
      `(setq ,*accumulation-variable*
             (sum ,*accumulation-variable* ,*it-var*))
      (call-next-method)))
    
(defmethod body-form ((clause sum-it-into-clause) end-tag)
  (declare (ignore end-tag))
  (if *it-var*
      `(setq ,(into-var clause)
             (sum ,(into-var clause) ,*it-var*))
      (call-next-method)))
