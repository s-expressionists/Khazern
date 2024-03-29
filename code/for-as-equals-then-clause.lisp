(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-EQUALS-THEN.

(defclass for-as-equals-then (for-as-subclause)
  ((%initial-form :initarg :initial-form :reader initial-form)
   (%subsequent-form :initarg :subsequent-form :reader subsequent-form)
   (%temp-tree :initarg :temp-tree :accessor temp-tree)
   (%directory :initarg :dictionary :accessor %directory)))

(defmethod initialize-instance :after
    ((clause for-as-equals-then) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (multiple-value-bind (temp-tree dictionary)
      (fresh-variables (var-spec clause))
    (reinitialize-instance clause
                           :temp-tree temp-tree
                           :dictionary dictionary)))
                         
(defmethod bound-variables ((subclause for-as-equals-then))
  (extract-variables (var-spec subclause)))

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
  (destructure-variables (temp-tree clause) (initial-form clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue-form.

(defmethod prologue-forms ((clause for-as-equals-then) end-tag)
  (declare (ignore end-tag))
  `((setq ,@(loop for (original . temp) in (%directory clause)
                  collect original
                  collect temp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step-bindings.

(defmethod step-bindings ((clause for-as-equals-then))
  (destructure-variables (temp-tree clause) (subsequent-form clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step-forms.

(defmethod step-forms ((clause for-as-equals-then))
  `((setq ,@(loop for (original . temp) in (%directory clause)
                  collect original
                  collect temp))))
