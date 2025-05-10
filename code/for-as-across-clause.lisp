(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-ACROSS

(defclass for-as-across (for-as-subclause)
  ((%vector-form :initarg :vector-form :reader vector-form)
   (%form-var :initform (gensym) :reader form-var)
   (%length-var :initform (gensym) :reader length-var)
   (%index-var :initform (gensym) :reader index-var)))

(defmethod map-variables (function (clause for-as-across))
  (map-variables function (var clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser for-as-across (:for-as-subclause)
  (consecutive (lambda (var-spec type-spec vector-form)
                 (make-instance 'for-as-across
                                :var (make-instance 'd-spec
                                                    :var-spec var-spec
                                                    :type-spec type-spec)
                                :vector-form vector-form))
               'd-var-spec
               'optional-type-spec
               (keyword :across)
               'terminal
               'anything))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute bindings.

(defmethod initial-bindings ((clause for-as-across))
  `((,(form-var clause) ,(vector-form clause))
    (,(index-var clause) 0)))

(defmethod final-bindings ((clause for-as-across))
  `((,(length-var clause) (length ,(form-var clause)))
    ,.(d-spec-outer-bindings (var clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute declarations.

(defmethod final-declarations ((clause for-as-across))
  (d-spec-outer-declarations (var clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute prologue-form.

(defmethod prologue-forms ((clause for-as-across))
  `(,@(termination-forms clause)
    ,@(d-spec-inner-form (var clause)
                         `(aref ,(form-var clause)
                                ,(index-var clause)))
    (incf ,(index-var clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute termination-forms

(defmethod termination-forms ((clause for-as-across))
  `((when (>= ,(index-var clause) ,(length-var clause))
      (go ,*end-tag*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute step-forms.

(defmethod step-forms ((clause for-as-across))
  `(,@(d-spec-inner-form (var clause)
                         `(aref ,(form-var clause)
                                ,(index-var clause)))
    (incf ,(index-var clause))))
