(cl:in-package #:khazern-elements)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;i
;;; ELEMENTS Path

(defclass for-as-elements (khazern::for-as-subclause)
  ((%form :accessor form
          :initarg :form)
   (%form-var :reader form-var
              :initform (gensym "FORM"))
   (%next-index-var :accessor next-index-var
                    :initform (gensym "INDEX"))
   (%index-var :accessor index-var
               :initarg :index-var
               :initform nil)
   (%start-form :accessor start-form
                :initform 0)
   (%start-var :reader start-var
               :initform (gensym "START"))
   (%end-form :accessor end-form)
   (%end-var :reader end-var
             :initform (gensym "END"))
   (%by-form :accessor by-form
             :initarg :by-form
             :initform 1)
   (%by-var :reader by-var
            :initform (gensym "BY"))))

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

(defmethod khazern::path-preposition-key ((instance for-as-elements) name)
  (cond ((member name '(:in :of) :test #'khazern::symbol-equal)
         :in)
        ((khazern::symbol-equal name :from)
         :from)
        ((khazern::symbol-equal name :to)
         :to)
        ((khazern::symbol-equal name :using)
         :using)
        ((khazern::symbol-equal name :by)
         :by)
        (t
         nil)))

(defmethod (setf khazern::path-preposition) (expression (instance for-as-elements) (key (eql :in)))
  (setf (form instance) expression))

(defmethod (setf khazern::path-preposition) (expression (instance for-as-elements) (key (eql :from)))
  (setf (start-form instance) expression))

(defmethod (setf khazern::path-preposition) (expression (instance for-as-elements) (key (eql :to)))
  (setf (end-form instance) expression))

(defmethod (setf khazern::path-preposition) (expression (instance for-as-elements) (key (eql :by)))
  (setf (by-form instance) expression))

(defmethod (setf khazern::path-preposition) (expression (instance for-as-elements) (key (eql :using)))
  (setf (index-var instance) (second expression))
  expression)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute bindings.

(defmethod khazern:initial-bindings ((clause for-as-elements))
  `((,(form-var clause) ,(form clause))
    (,(next-index-var clause) ,(start-form clause))
    (,(by-var clause) ,(by-form clause))
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
    (incf ,(next-index-var clause) ,(by-var clause))))

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
  `((incf ,(next-index-var clause) ,(by-var clause))))
