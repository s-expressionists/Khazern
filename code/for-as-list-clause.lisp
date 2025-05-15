(cl:in-package #:khazern)

(defclass for-as-list (for-as-subclause form-mixin form-var-mixin)
  ((%by-form :reader by-form
             :initarg :by-form)
   (%by-var :reader by-var
            :initform (gensym))
   (%rest-var :reader rest-var
              :initform (gensym))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-IN-LIST.

(defclass for-as-in-list (for-as-list) ())

(defmethod map-variables (function (clause for-as-list))
  (map-variables function (var clause)))

(define-parser for-as-list-by-parser ()
  (optional '#'cdr
            (consecutive #'identity
                         (keyword :by)
                         'anything)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser for-as-in-list-parser (:for-as-subclause)
  (consecutive (lambda (var type-spec form by-form)
                 (make-instance 'for-as-in-list
                                :var (make-instance 'd-spec
                                                    :var-spec var
                                                    :type-spec type-spec)
                                :form form
                                :by-form by-form))
               'd-var-spec
               'optional-type-spec
               (keyword :in)
               'terminal
               'anything
               'for-as-list-by-parser))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-ON-LIST.

(defclass for-as-on-list (for-as-list) ())

(define-parser for-as-on-list-parser (:for-as-subclause)
  (consecutive (lambda (var type-spec form by-form)
                 (make-instance 'for-as-on-list
                                :var (make-instance 'd-spec
                                                    :var-spec var
                                                    :type-spec type-spec)
                                :form form
                                :by-form by-form))
               'd-var-spec
               'optional-type-spec
               (keyword :on)
               'terminal
               'anything
               'for-as-list-by-parser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the bindings.

(defmethod initial-bindings ((clause for-as-list))
  `((,(form-var clause) ,(form clause))
    ,@(if (member (by-form clause) '(#'cdr #'cddr) :test #'equal)
          '()
          `((,(by-var clause) ,(by-form clause))))))

(defmethod final-bindings ((clause for-as-list))
  `((,(rest-var clause) ,(form-var clause))
    ,.(d-spec-outer-bindings (var clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the declarations.

(defmethod final-declarations ((clause for-as-list))
  (d-spec-outer-declarations (var clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the initial step forms.

(defmethod initial-step-forms ((clause for-as-in-list))
  `((when (endp ,(rest-var clause))
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var clause) `(car ,(rest-var clause)))))

(defmethod initial-step-forms ((clause for-as-on-list))
  `((when (atom ,(rest-var clause))
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var clause) (rest-var clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the subsequent step forms.

(defmethod subsequent-step-forms ((clause for-as-in-list))
  `(,(if (member (by-form clause) '(#'cdr #'cddr) :test #'equal)
         `(setq ,(rest-var clause)
                (,(cadr (by-form clause)) ,(rest-var clause)))
         `(setq ,(rest-var clause)
                (funcall ,(by-var clause) ,(rest-var clause))))
    (when (endp ,(rest-var clause))
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var clause) `(car ,(rest-var clause)))))

(defmethod subsequent-step-forms ((clause for-as-on-list))
  `(,(if (member (by-form clause) '(#'cdr #'cddr) :test #'equal)
         `(setq ,(rest-var clause)
                (,(cadr (by-form clause)) ,(rest-var clause)))
         `(setq ,(rest-var clause)
                (funcall ,(by-var clause) ,(rest-var clause))))
    (when (atom ,(rest-var clause))
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var clause) (rest-var clause))))
