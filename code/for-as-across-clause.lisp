(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-ACROSS

(defclass for-as-across (for-as-subclause form-mixin form-var-mixin)
  ((%length-var :reader length-var
                :initform (gensym "LENGTH"))
   (%index-var :reader index-var
               :initform (gensym "INDEX"))))

(defmethod map-variables (function (clause for-as-across))
  (map-variables function (var clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser for-as-across (:for-as-subclause)
  (consecutive (lambda (var-spec type-spec form)
                 (make-instance 'for-as-across
                                :var (make-instance 'd-spec
                                                    :var-spec var-spec
                                                    :type-spec type-spec)
                                :form form))
               'd-var-spec
               'optional-type-spec
               (keyword :across)
               'terminal
               'anything))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute bindings.

(defmethod initial-bindings ((clause for-as-across))
  `((,(form-var clause) ,(form clause))
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

(defmethod initial-step-forms ((clause for-as-across))
  `((when (>= ,(index-var clause) ,(length-var clause))
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var clause)
                         `(aref ,(form-var clause)
                                ,(index-var clause)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute step-forms.

(defmethod subsequent-step-forms ((clause for-as-across))
  `((when (>= (incf ,(index-var clause)) ,(length-var clause))
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var clause)
                         `(aref ,(form-var clause)
                                ,(index-var clause)))))
