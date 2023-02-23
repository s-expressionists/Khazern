(cl:in-package #:khazern)

(defclass repeat-clause (termination-test-clause var-and-type-spec-mixin)
  ((%form :initarg :form :reader form))
  (:default-initargs :var-spec (gensym) :type-spec 'real))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser repeat-clause (:body-clause)
  (consecutive (lambda (repeat form)
                 (declare (ignore repeat))
                 (make-instance 'repeat-clause :form form))
               (keyword 'repeat)
               'anything))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the bindings.

(defmethod initial-bindings ((clause repeat-clause))
  `((,(var-spec clause) ,(form clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the declarations.

(defmethod initial-declarations ((clause repeat-clause))
  `((cl:type number ,(var-spec clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue-form.

(defmethod prologue-form ((clause repeat-clause) end-tag)
  `(when (<= ,(var-spec clause) 0)
     (go ,end-tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the termination-form.

(defmethod termination-form ((clause repeat-clause) end-tag)
  `(when (<= ,(var-spec clause) 1)
     (go ,end-tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step-form.

(defmethod step-form ((clause repeat-clause))
  `(decf ,(var-spec clause)))
