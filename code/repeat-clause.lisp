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
  `((,(var-spec clause) (ceiling ,(form clause)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the declarations.

(defmethod initial-declarations ((clause repeat-clause))
  `((cl:type ,(type-spec clause) ,(var-spec clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue-form.

(defmethod prologue-forms ((clause repeat-clause) end-tag)
  `((when (<= ,(var-spec clause) 0)
      (go ,end-tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the termination-forms.

(defmethod termination-forms ((clause repeat-clause) end-tag)
  `((when (<= ,(var-spec clause) 1)
      (go ,end-tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step-forms.

(defmethod step-forms ((clause repeat-clause))
  `((decf ,(var-spec clause))))
