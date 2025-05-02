(cl:in-package #:khazern)

(defclass maximize-clause (max/min-accumulation-clause form-mixin)
  ())

(defclass maximize-it-clause (maximize-clause it-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser maximize-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var type-spec
                        &aux (itp (it-keyword-p form)))
                 (cond ((and itp var)
                        (make-instance 'maximize-it-clause
                                       :form form
                                       :into-var var
                                       :type-spec (type-or-null type-spec)))
                       (itp
                        (make-instance 'maximize-it-clause
                                       :form form
                                       :type-spec (type-or-null type-spec)))
                       (var
                        (make-instance 'maximize-clause
                                       :form form
                                       :into-var var
                                       :type-spec (type-or-null type-spec)))
                       (t
                        (make-instance 'maximize-clause
                                       :form form
                                       :type-spec (type-or-null type-spec)))))
               (keyword :maximize :maximizing)
               'terminal
               'anything
               'optional-into-phrase
               'optional-type-spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.

(defmethod body-forms ((clause maximize-clause) end-tag)
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
      `((cond ((null ,(into-var clause))
               (setq ,(into-var clause) ,*it-var*)
               (unless (realp ,(into-var clause))
                 (error 'type-error :datum ,(into-var clause)
                                    :expected-type 'real)))
              (t
               (setq ,(into-var clause)
                     (max ,(into-var clause) ,*it-var*)))))
      (call-next-method)))
