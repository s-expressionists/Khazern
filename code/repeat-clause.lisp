(cl:in-package #:khazern)

(defclass repeat-clause (termination-test-clause var-and-type-spec-mixin)
  ((%form :initarg :form :reader form))
  (:default-initargs :var-spec (gensym "REPEAT")
                     :type-spec 'fixnum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser repeat-clause (:body-clause)
  (consecutive (lambda (form)
                 (make-instance 'repeat-clause :form form))
               (keyword :repeat)
               'terminal
               'anything))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the bindings.

(defmethod initial-bindings ((clause repeat-clause))
  `((,(var-spec clause) (max 0 (ceiling ,(form clause))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the declarations.

(defmethod initial-declarations ((clause repeat-clause))
  `((cl:type ,(type-spec clause) ,(var-spec clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the initial step forms.

(defmethod initial-step-forms ((clause repeat-clause))
  `((when (zerop ,(var-spec clause))
      (go ,*epilogue-tag*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the subsequent step forms.

(defmethod subsequent-step-forms ((clause repeat-clause))
  `((when (zerop (decf ,(var-spec clause)))
      (go ,*epilogue-tag*))))
