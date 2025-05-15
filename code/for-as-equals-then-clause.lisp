(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-EQUALS-THEN.

(defclass for-as-equals-then (for-as-subclause)
  ((%initial-form :reader initial-form
                  :initarg :initial-form)
   (%subsequent-form :reader subsequent-form
                     :initarg :subsequent-form)))

(defmethod map-variables (function (clause for-as-equals-then))
  (map-variables function (var clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser for-as-equals-then-parser (:for-as-subclause)
  (consecutive (lambda (var-spec type-spec form1 initargs)
                 (apply #'make-instance 'for-as-equals-then
                        :var (make-instance 'd-spec
                                            :var-spec var-spec
                                            :type-spec type-spec
                                            :temp-var t)
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
  (d-spec-outer-bindings (var clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the declarations.

(defmethod initial-declarations ((clause for-as-equals-then))
  (d-spec-outer-declarations (var clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue-bindings.

(defmethod initial-step-bindings ((clause for-as-equals-then))
  (d-spec-inner-bindings (var clause) (initial-form clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue-form.

(defmethod initial-step-forms ((clause for-as-equals-then))
  `((setq ,@(d-spec-inner-assignments (var clause) (initial-form clause)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step-bindings.

(defmethod subsequent-step-bindings ((clause for-as-equals-then))
  (d-spec-inner-bindings (var clause) (subsequent-form clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step-forms.

(defmethod subsequent-step-forms ((clause for-as-equals-then))
  `((setq ,@(d-spec-inner-assignments (var clause) (subsequent-form clause)))))
