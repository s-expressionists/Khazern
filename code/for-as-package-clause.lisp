(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-PACKAGE

(defclass for-as-package (for-as-subclause form-mixin form-var-mixin)
  ((%temp-entry-p-var :reader temp-entry-p-var
                      :initform (gensym))
   (%temp-symbol-var :reader temp-symbol-var
                     :initform (gensym))
   (%iterator-var :reader iterator-var
                  :initform (gensym))
   (%iterator-keywords :reader iterator-keywords
                       :initarg :iterator-keywords))
  (:default-initargs :form '*package*))

(defmethod map-variables (function (clause for-as-package))
  (map-variables function (var clause)))

(defun make-for-as-package-symbol (name var-spec type-spec)
  (declare (ignore name))
  (make-instance 'for-as-package
                 :var (make-instance 'd-spec
                                     :var-spec var-spec
                                     :type-spec type-spec)
                 :iterator-keywords '(:internal :external :inherited)))

(defun make-for-as-package-present-symbol (name var-spec type-spec)
  (declare (ignore name))
  (make-instance 'for-as-package
                 :var (make-instance 'd-spec
                                     :var-spec var-spec
                                     :type-spec type-spec)
                 :iterator-keywords '(:internal :external)))

(defun make-for-as-package-external-symbol (name var-spec type-spec)
  (declare (ignore name))
  (make-instance 'for-as-package
                 :var (make-instance 'd-spec
                                     :var-spec var-spec
                                     :type-spec type-spec)
                 :iterator-keywords '(:external)))

(defmethod path-preposition-names ((instance for-as-package))
  '((:in . :in) (:of . :in)))

(defmethod (setf path-preposition) (expression (instance for-as-package) (key (eql :in)))
  (setf (form instance) expression))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the initial bindings.

(defmethod initial-bindings ((clause for-as-package))
  `((,(form-var clause) ,(form clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the subclause wrapper.

(defmethod wrap-subclause ((subclause for-as-package) inner-form)
  (wrap-let `((,(temp-entry-p-var subclause) nil)
              (,(temp-symbol-var subclause) nil)
              ,.(d-spec-outer-bindings (var subclause)))
            (d-spec-outer-declarations (var subclause))
            `((with-package-iterator
                  (,(iterator-var subclause)
                   ,(form-var subclause)
                   ,@(iterator-keywords subclause))
                ,@inner-form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue form.

(defmethod initial-step-forms ((subclause for-as-package))
  `((multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-symbol-var subclause))
      (,(iterator-var subclause)))
    (unless ,(temp-entry-p-var subclause)
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var subclause)
                         (temp-symbol-var subclause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step form.

(defmethod subsequent-step-forms ((subclause for-as-package))
  `((multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-symbol-var subclause))
      (,(iterator-var subclause)))
    (unless ,(temp-entry-p-var subclause)
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var subclause)
                         (temp-symbol-var subclause))))
