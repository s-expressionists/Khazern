(cl:in-package #:khazern-elements)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ELEMENTS Path

(defclass for-as-elements (khazern::for-as-subclause)
  ((%form :initarg :form :reader form)
   (%form-var :initform (gensym) :reader form-var)
   (%length-var :initform (gensym) :reader length-var)
   (%index-var :initarg :index-var :initform (gensym) :reader index-var)))

(defmethod khazern:map-variables (function (clause for-as-elements))
  (khazern:map-variables function (khazern:var clause)))

(defun make-elements-path (name var-spec type-spec data
                           &key (in nil inp) using)
  (declare (ignore name data))
  (let (index)
    (do ((head using (cddr head)))
        ((null head))
      (cond ((khazern::symbol-equal (car head) :index)
             (setf index (second head)))))
    (make-instance 'for-as-elements
                   :form in
                   :var (make-instance 'khazern:d-spec
                                       :var-spec var-spec
                                       :type-spec type-spec)
                   :index-var (or index (gensym "INDEX")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute bindings.

(defmethod khazern:initial-bindings ((clause for-as-elements))
  `((,(form-var clause) ,(form clause))
    (,(index-var clause) 0)))

(defmethod khazern:final-bindings ((clause for-as-elements))
  `((,(length-var clause) (length ,(form-var clause)))
    ,.(khazern:d-spec-outer-bindings (khazern:var clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute declarations.

(defmethod khazern:final-declarations ((clause for-as-elements))
  (khazern:d-spec-outer-declarations (khazern:var clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute prologue-form.

(defmethod khazern:prologue-forms ((clause for-as-elements) end-tag)
  `(,@(khazern:termination-forms clause end-tag)
    ,@(khazern:d-spec-inner-form (khazern:var clause)
                         `(elt ,(form-var clause)
                               ,(index-var clause)))
    (incf ,(index-var clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute termination-forms

(defmethod khazern:termination-forms ((clause for-as-elements) end-tag)
  `((when (>= ,(index-var clause) ,(length-var clause))
      (go ,end-tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute step-forms.

(defmethod khazern:step-forms ((clause for-as-elements))
  `(,@(khazern:d-spec-inner-form (khazern:var clause)
                                 `(elt ,(form-var clause)
                                       ,(index-var clause)))
    (incf ,(index-var clause))))
