(cl:in-package #:khazern)

(defclass minimize-clause (max/min-accumulation-clause form-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser minimize-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var type-spec)
                 (make-instance 'minimize-clause
                                :form form
                                :var (make-instance 'd-spec
                                                    :var-spec var
                                                    :type-spec (type-or-null type-spec))))
               (keyword :minimize :minimizing)
               'terminal
               'anything
               'optional-into-phrase
               'optional-type-spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.

(defmethod body-forms ((clause minimize-clause))
  (let ((form (form clause)))
    (when (and *it-var* (it-keyword-p form))
      (setf form *it-var*))
    `((cond ((null ,(var-spec (var clause)))
             (setq ,(var-spec (var clause)) ,form)
             (unless (realp ,(var-spec (var clause)))
               (error 'type-error :datum ,(var-spec (var clause))
                                  :expected-type 'real)))
            (t
             (setq ,(var-spec (var clause))
                   (min ,(var-spec (var clause)) ,form)))))))
