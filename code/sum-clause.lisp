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

(define-parser sum-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var type-spec
                        &aux (itp (it-keyword-p form)))
                 (cond ((and itp var)
                        (make-instance 'sum-it-into-clause
                                       :form form
                                       :into-var var
                                       :type-spec type-spec))
                       (itp
                        (make-instance 'sum-it-clause
                                       :form form
                                       :type-spec type-spec))
                       (var
                        (make-instance 'sum-form-into-clause
                                       :form form
                                       :into-var var
                                       :type-spec type-spec))
                       (t
                        (make-instance 'sum-form-clause
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
