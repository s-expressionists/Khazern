(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-EQUALS-THEN.

(defclass for-as-equals-then (for-as-subclause)
  ((%initial-form :initarg :initial-form :reader initial-form)
   (%subsequent-form :initarg :subsequent-form :reader subsequent-form)))

(defmethod initialize-instance :after
    ((clause for-as-equals-then) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (temps clause) (var-spec-temps (var-spec clause) t)))

(defmethod map-variables (function (clause for-as-equals-then))
  (%map-variables function (var-spec clause) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser for-as-equals-then-parser (:for-as-subclause)
  (consecutive (lambda (var-spec type-spec form1 initargs)
                 (apply #'make-instance 'for-as-equals-then
                        :var-spec var-spec
                        :type-spec type-spec
                        :initial-form form1
                        (or initargs (cl:list :subsequent-form form1))))
               'd-var-spec
               'optional-type-spec
               (keyword :=)
               'terminal
               'anything
               (optional nil
                         (consecutive (lambda (form)
                                        (cl:list :subsequent-form form))
                                      (keyword :then)
                                      'terminal
                                      'anything))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the bindings.

(defmethod initial-bindings ((clause for-as-equals-then))
  (generate-variable-bindings (var-spec clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the declarations.

(defmethod initial-declarations ((clause for-as-equals-then))
  (generate-variable-declarations (var-spec clause) (type-spec clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue-bindings.

(defmethod prologue-bindings ((clause for-as-equals-then))
  (var-spec-bindings (var-spec clause) (initial-form clause) (temps clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue-form.

(defmethod prologue-forms ((clause for-as-equals-then) end-tag)
  (declare (ignore end-tag))
  `((setq ,@(var-spec-assignments (var-spec clause) (initial-form clause) (temps clause)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step-bindings.

(defmethod step-bindings ((clause for-as-equals-then))
  (var-spec-bindings (var-spec clause) (subsequent-form clause) (temps clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step-forms.

(defmethod step-forms ((clause for-as-equals-then))
  `((setq ,@(var-spec-assignments (var-spec clause) (subsequent-form clause) (temps clause)))))
