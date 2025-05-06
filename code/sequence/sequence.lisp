(in-package #:khazern-sequence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-OVER

(defclass for-as-over ()
  ((%var :initarg :var :accessor var)
   (%other-var :initarg :other-var :accessor other-var)
   (%sequence-form :initarg :sequence-form :reader sequence-form)
   (%form-var :initform (gensym) :reader form-var)
   (%length-var :initform (gensym) :reader length-var)
   (%state-var :initform (gensym) :reader state-var)
   (%limit-var :initform (gensym) :reader limit-var)
   (%from-end-var :initform (gensym) :reader from-end-var)
   (%step-var :initform (gensym) :reader step-var)
   (%endp-var :initform (gensym) :reader endp-var)
   (%read-var :initform (gensym) :reader read-var)
   (%write-var :initform (gensym) :reader write-var)
   (%index-var :initform (gensym) :reader index-var)))

(defmethod khazern:main-clause-p ((clause for-as-over))
  t)

(defmethod khazern:variable-clause-p ((clause for-as-over))
  t)

(defmethod khazern:map-variables (function (clause for-as-over))
  (khazern:%map-variables function (var-spec clause) (type-spec clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(khazern:define-parser for-as-over (:for-as-subclause)
  (khazern:consecutive (lambda (var type-spec other-var sequence-form)
                         (make-instance 'for-as-over
                                        :var (make-instance 'khazern:d-spec
                                                            :var-spec var
                                                            :type-spec type-spec)
                                        :other-var (when other-var
                                                     (make-instance 'khazern:d-spec
                                                                    :var other-var))
                                        :sequence-form sequence-form))
                       'khazern:anything
                       'khazern:optional-type-spec
                       (khazern:optional nil
                                         (khazern:consecutive (lambda (other-var)
                                                                other-var)
                                                              (khazern:keyword :at)
                                                              'khazern:anything))
                       (khazern:keyword :over)
                       'khazern:terminal
                       'khazern:anything))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute bindings.

(defmethod khazern:initial-bindings ((clause for-as-over))
  `((,(form-var clause) ,(sequence-form clause))
    ,(state-var clause)
    ,(from-end-var clause)
    ,(limit-var clause)
    ,(step-var clause)
    ,(endp-var clause)
    ,(read-var clause)
    ,(write-var clause)
    ,(index-var clause)))

(defmethod khazern:final-bindings ((clause for-as-over))
  (nconc (khazern:d-spec-outer-bindings (var clause))
         (when (other-var clause)
           (khazern:d-spec-outer-bindings (other-var clause)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute declarations.

(defmethod khazern:initial-declarations ((clause for-as-over))
  `((ignorable ,(write-var clause) ,(index-var clause))))

(defmethod khazern:final-declarations ((clause for-as-over))
  (khazern:d-spec-outer-declarations (var clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute prologue-forms.

(defmethod khazern:prologue-forms ((clause for-as-over) end-tag)
  `((multiple-value-setq (,(state-var clause) ,(limit-var clause) ,(from-end-var clause)
                          ,(step-var clause) ,(endp-var clause) ,(read-var clause)
                          ,(write-var clause) ,(index-var clause))
      (sequence:make-sequence-iterator ,(form-var clause)))
    ,@(khazern:termination-forms clause end-tag)
    ,@(khazern:d-spec-inner-form (var clause)
                                 `(funcall ,(read-var clause) ,(form-var clause)
                                           ,(state-var clause)))
    ,@(when (other-var clause)
        (khazern:d-spec-inner-form (other-var clause)
				   `(funcall ,(index-var clause) ,(form-var clause)
					     ,(state-var clause))))
    (setf ,(state-var clause)
          (funcall ,(step-var clause) ,(form-var clause) ,(state-var clause) ,(from-end-var clause)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute termination-forms

(defmethod khazern:termination-forms ((clause for-as-over) end-tag)
  `((when (funcall ,(endp-var clause) ,(form-var clause) ,(state-var clause)
                   ,(limit-var clause) ,(from-end-var clause))
      (go ,end-tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute step-forms.

(defmethod khazern:step-forms ((clause for-as-over))
  `(,@(khazern:d-spec-inner-form (var clause)
                                 `(funcall ,(read-var clause) ,(form-var clause)
                                           ,(state-var clause)))
    ,.(when (other-var clause)
        (khazern:d-spec-inner-form (other-var clause)
                                   `(funcall ,(index-var clause) ,(form-var clause)
                                             ,(state-var clause))))
    (setf ,(state-var clause)
          (funcall ,(step-var clause) ,(form-var clause)
                   ,(state-var clause) ,(from-end-var clause)))))
