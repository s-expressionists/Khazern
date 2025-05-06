(cl:in-package #:khazern)

(defclass for-as-list (for-as-subclause)
  ((%list-form :initarg :list-form :reader list-form)
   (%list-var :initform (gensym) :reader list-var)
   (%by-form :initarg :by-form :reader by-form)
   (%by-var :initform (gensym) :reader by-var)
   (%rest-var :initform (gensym) :reader rest-var)))

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
  (consecutive (lambda (var type-spec list-form by-form)
                 (make-instance 'for-as-in-list
                                :var (make-instance 'd-spec
                                                    :var-spec var
                                                    :type-spec type-spec)
                                :list-form list-form
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
  (consecutive (lambda (var type-spec list-form by-form)
                 (make-instance 'for-as-on-list
                                :var (make-instance 'd-spec
                                                    :var-spec var
                                                    :type-spec type-spec)
                                :list-form list-form
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
  `((,(list-var clause) ,(list-form clause))
    ,@(if (member (by-form clause) '(#'cdr #'cddr) :test #'equal)
          '()
          `((,(by-var clause) ,(by-form clause))))))

(defmethod final-bindings ((clause for-as-list))
  `((,(rest-var clause) ,(list-var clause))
    ,.(d-spec-generate-variable-bindings (var clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the declarations.

(defmethod final-declarations ((clause for-as-list))
  (d-spec-generate-variable-declarations (var clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue.

(defmethod prologue-forms ((clause for-as-in-list) end-tag)
  `(,@(termination-forms clause end-tag)
    ,@(d-spec-generate-assignments (var clause) `(car ,(rest-var clause)))
    ,(if (member (by-form clause) '(#'cdr #'cddr) :test #'equal)
         `(setq ,(rest-var clause)
                (,(cadr (by-form clause)) ,(rest-var clause)))
         `(setq ,(rest-var clause)
                (funcall ,(by-var clause) ,(rest-var clause))))))

(defmethod prologue-forms ((clause for-as-on-list) end-tag)
  `(,@(termination-forms clause end-tag)
    ,@(d-spec-generate-assignments (var clause) (rest-var clause))
    ,(if (member (by-form clause) '(#'cdr #'cddr) :test #'equal)
         `(setq ,(rest-var clause)
                (,(cadr (by-form clause)) ,(rest-var clause)))
         `(setq ,(rest-var clause)
                (funcall ,(by-var clause) ,(rest-var clause))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the termination-forms.

(defmethod termination-forms ((clause for-as-in-list) end-tag)
  `((when (endp ,(rest-var clause))
      (go ,end-tag))))

(defmethod termination-forms ((clause for-as-on-list) end-tag)
  `((when (atom ,(rest-var clause))
      (go ,end-tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step-forms.

(defmethod step-forms ((clause for-as-in-list))
  `(,@(d-spec-generate-assignments (var clause) `(car ,(rest-var clause)))
    ,(if (member (by-form clause) '(#'cdr #'cddr) :test #'equal)
         `(setq ,(rest-var clause)
                (,(cadr (by-form clause)) ,(rest-var clause)))
         `(setq ,(rest-var clause)
                (funcall ,(by-var clause) ,(rest-var clause))))))

(defmethod step-forms ((clause for-as-on-list))
  `(,@(d-spec-generate-assignments (var clause) (rest-var clause))
    ,(if (member (by-form clause) '(#'cdr #'cddr) :test #'equal)
         `(setq ,(rest-var clause)
                (,(cadr (by-form clause)) ,(rest-var clause)))
         `(setq ,(rest-var clause)
                (funcall ,(by-var clause) ,(rest-var clause))))))
