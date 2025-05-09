(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-PACKAGE

(defclass for-as-package (for-as-subclause)
  ((%package-form :initarg :package-form :accessor package-form :initform '*package*)
   (%package-var :initform (gensym) :reader package-var)
   (%temp-entry-p-var :initform (gensym) :reader temp-entry-p-var)
   (%temp-symbol-var :initform (gensym) :reader temp-symbol-var)
   (%iterator-var :initform (gensym) :reader iterator-var)
   (%iterator-keywords :initarg :iterator-keywords :reader iterator-keywords)))

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

(defmethod path-preposition-p ((instance for-as-package) name)
  (member name '(:in :of) :test #'symbol-equal))

(defmethod (setf path-preposition) (expression (instance for-as-package) name)
  (cond ((member name '(:in :of) :test #'symbol-equal)
         (setf (package-form instance) expression))
        (t
         (error "Unknown path preposition ~a" name)))
  expression)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the initial bindings.

(defmethod initial-bindings ((clause for-as-package))
  `((,(package-var clause) ,(package-form clause))))

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
                   ,(package-var subclause)
                   ,@(iterator-keywords subclause))
                ,@inner-form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue form.

(defmethod prologue-forms ((subclause for-as-package) end-tag)
  `((multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-symbol-var subclause))
      (,(iterator-var subclause)))
    (unless ,(temp-entry-p-var subclause)
      (go ,end-tag))
    ,@(d-spec-inner-form (var subclause)
                         (temp-symbol-var subclause))
    (multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-symbol-var subclause))
      (,(iterator-var subclause)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the termination form.

(defmethod termination-forms ((subclause for-as-package) end-tag)
  `((unless ,(temp-entry-p-var subclause)
      (go ,end-tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step form.

(defmethod step-forms ((subclause for-as-package))
  `(,@(d-spec-inner-form (var subclause)
                         (temp-symbol-var subclause))
    (multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-symbol-var subclause))
      (,(iterator-var subclause)))))
