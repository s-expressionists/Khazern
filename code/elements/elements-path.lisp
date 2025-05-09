(cl:in-package #:khazern-elements)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;i
;;; ELEMENTS Path

(defclass for-as-elements (khazern::for-as-subclause)
  ((%form :initarg :form :accessor form)
   (%form-var :initform (gensym) :reader form-var)
   (%next-index-var :initform (gensym "INDEX")
               :accessor next-index-var)
   (%index-var :initarg :index-var :initform nil
               :accessor index-var)
   (%start-form :initform 0 :accessor start-form)
   (%start-var :initform (gensym "START") :reader start-var)
   (%end-form :accessor end-form)
   (%end-var :initform (gensym "END") :reader end-var)
   (%by-form :initform 1 :initarg :by-form :accessor by-form)
   (%by-var :initform (gensym) :reader by-var)))

(defmethod initialize-instance :after ((instance for-as-elements) &rest initargs &key)
  (declare (ignore initargs))
  (setf (end-form instance)
        `(length ,(form-var instance))))

(defmethod khazern:map-variables (function (clause for-as-elements))
  (khazern:map-variables function (khazern:var clause))
  #+(or)(when (index-var clause)
    (funcall function (index-var) 'fixnum nil)))

(defun make-elements-path (name var-spec type-spec)
  (declare (ignore name))
  (make-instance 'for-as-elements
                 :var (make-instance 'khazern:d-spec
                                     :var-spec var-spec
                                     :type-spec type-spec)))

(defmethod khazern::path-preposition-p ((instance for-as-elements) name)
  (member name '(:in :of :using :from :to) :test #'khazern::symbol-equal))

(defmethod (setf khazern::path-preposition) (expression (instance for-as-elements) name)
  (cond ((member name '(:in :of) :test #'khazern::symbol-equal)
         (setf (form instance) expression))
        ((member name '(:from) :test #'khazern::symbol-equal)
         (setf (start-form instance) expression))
        ((member name '(:to) :test #'khazern::symbol-equal)
         (setf (end-form instance) expression))
        ((khazern::symbol-equal name :using)
         (setf (index-var instance) (second expression)))
        (t
         (error "Unknown path preposition ~a" name)))
  expression)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute bindings.

(defmethod khazern:initial-bindings ((clause for-as-elements))
  `((,(form-var clause) ,(form clause))
    (,(next-index-var clause) ,(start-form clause))
    ,@(when (index-var clause)
        `((,(index-var clause) nil)))))

(defmethod khazern:final-bindings ((clause for-as-elements))
  (list* `(,(end-var clause) ,(end-form clause))
         (khazern:d-spec-outer-bindings (khazern:var clause))))

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
    (incf ,(next-index-var clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute termination-forms

(defmethod khazern:termination-forms ((clause for-as-elements) end-tag)
  (nconc `((when (>= ,(next-index-var clause) ,(end-var clause))
             (go ,end-tag)))
         (khazern:d-spec-inner-form (khazern:var clause)
                                    `(elt ,(form-var clause)
                                          ,(next-index-var clause)))
         (when (index-var clause)
           `((setq ,(index-var clause) ,(next-index-var clause))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute step-forms.

(defmethod khazern:step-forms ((clause for-as-elements))
  `((incf ,(next-index-var clause))))
