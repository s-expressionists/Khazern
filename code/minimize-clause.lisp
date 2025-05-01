(cl:in-package #:khazern)

(defclass minimize-clause (max/min-accumulation-clause form-mixin)
  ())

(defclass minimize-it-clause (minimize-clause it-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser minimize-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var type-spec
                        &aux (itp (it-keyword-p form)))
                 (cond ((and itp var)
                        (make-instance 'minimize-it-clause
                                       :form form
                                       :into-var var
                                       :type-spec type-spec))
                       (itp
                        (make-instance 'minimize-it-clause
                                       :form form
                                       :type-spec type-spec))
                       (var
                        (make-instance 'minimize-clause
                                       :form form
                                       :into-var var
                                       :type-spec type-spec))
                       (t
                        (make-instance 'minimize-clause
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

(defmethod body-forms ((clause minimize-clause) end-tag)
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
      `((cond ((null ,(into-var clause))
               (setq ,(into-var clause) ,*it-var*)
               (unless (realp ,(into-var clause))
                 (error 'type-error :datum ,(into-var clause)
                                    :expected-type 'real)))
              (t
               (setq ,(into-var clause)
                     (min ,(into-var clause) ,*it-var*)))))
      (call-next-method)))
