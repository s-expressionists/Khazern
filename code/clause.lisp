(cl:in-package #:khazern)

;;;; The terminology used here is that of the BNF grammar in the
;;;; dictionary description of the loop macro in the HyperSpec.  It is
;;;; not the same as the terminology used in the section 6.1.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Common classes.

;;; The base class of all clauses.
(defclass clause () ())

;;; Mixin for clauses that accept `AND'.
(defclass subclauses-mixin ()
  ((%subclauses :initarg :subclauses :reader subclauses)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Expansion methods for FOR-AS clause.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the bindings.

(defmethod initial-bindings ((clause subclauses-mixin))
  (mapcan #'initial-bindings (subclauses clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the declarations.

(defmethod initial-declarations ((clause subclauses-mixin))
  (mapcan #'initial-declarations (subclauses clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue-forms.

(defmethod prologue-forms ((clause subclauses-mixin))
  (mapcan #'prologue-forms (subclauses clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step forms.

(defmethod initial-step-forms ((clause subclauses-mixin))
  (wrap-let* (mapcan #'initial-step-bindings (subclauses clause))
             (mapcan #'initial-step-declarations (subclauses clause))
             (mapcan #'initial-step-forms (subclauses clause))))

(defmethod subsequent-step-forms ((clause subclauses-mixin))
  (wrap-let* (mapcan #'subsequent-step-bindings (subclauses clause))
             (mapcan #'subsequent-step-declarations (subclauses clause))
             (mapcan #'subsequent-step-forms (subclauses clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the body-forms.

(defmethod body-forms ((clause subclauses-mixin))
  (mapcan #'body-forms (subclauses clause)))

(defmethod epilogue-forms ((clause subclauses-mixin))
  (mapcan #'epilogue-forms (subclauses clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Step a FOR-AS clause.

;;; Mixin for clauses and subclauses that take
;;; a VAR-SPEC and a TYPE-SPEC.
(defclass var-and-type-spec-mixin ()
  ((%var-spec :initarg :var-spec :accessor var-spec)
   (%type-spec :initarg :type-spec :accessor type-spec)
   (%temps :initarg :temps :accessor temps)))

(defclass var-mixin ()
  ((%var :initarg :var :accessor var)))

;;; Mixin for clauses that take a list of compound forms.
(defclass compound-forms-mixin ()
  ((%forms :initarg :forms :reader forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin for clauses that make the loop return a value.

(defclass loop-return-clause-mixin () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin for clauses that has an implicit IT argument.

(defclass it-mixin () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin for clauses that has an explicit form argument.

(defclass form-mixin ()
  ((%form :initarg :form :reader form)))
