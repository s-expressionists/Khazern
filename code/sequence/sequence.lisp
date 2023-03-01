(in-package #:khazern-sequence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-OVER

(defclass for-as-over ()
  ((%var-spec :initarg :var-spec :accessor var-spec)
   (%type-spec :initarg :type-spec :accessor type-spec)
   ;; This slot contains a copy of the tree contained in the VAR-SPEC
   ;; slot except that the non-NIL leaves have been replaced by
   ;; GENSYMs.
   (%temp-vars :initarg :temp-vars :reader temp-vars)
   ;; This slot contains a list of pairs.  Each pair is a CONS cell
   ;; where the CAR is a variable in VAR-SPEC and the CDR is the
   ;; corresponding variable in TEMP-VARS.
   (%other-var :initarg :other-var :reader other-var)
   (%dictionary :initarg :dictionary :reader dictionary)
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

(defmethod initialize-instance :after
    ((clause for-as-over) &key &allow-other-keys)
  (multiple-value-bind (temp-vars dictionary)
      (khazern:fresh-variables (var-spec clause))
    (reinitialize-instance clause
                           :temp-vars temp-vars
                           :dictionary dictionary)))

(defmethod khazern:main-clause-p ((clause for-as-over))
  t)

(defmethod khazern:variable-clause-p ((clause for-as-over))
  t)

;;; The FOR-AS-OVER clasue binds all the variables in the VAR-SPEC
;;; of the clause, so this method should return a list of all those
;;; variables.
(defmethod khazern:bound-variables ((clause for-as-over))
  (khazern:extract-variables (var-spec clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(khazern:define-parser for-as-over (:for-as-subclause)
  (khazern:consecutive (lambda (var type-spec other-var across sequence-form)
                         (declare (ignore across))
                         (make-instance 'for-as-over
                                        :var-spec var
                                        :type-spec type-spec
                                        :other-var other-var
                                        :sequence-form sequence-form))
                       'khazern:anything
                       'khazern:optional-type-spec
                       (khazern:optional nil
                                         (khazern:consecutive (lambda (at other-var)
                                                                (declare (ignore at))
                                                                other-var)
                                                              (khazern:keyword 'at)
                                                              'khazern:anything))
                       (khazern:keyword 'over)
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
  `(,@(when (other-var clause)
        `(,(other-var clause)))
    ,@(mapcar (lambda (entry)
                `(,(car entry) nil))
              (dictionary clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute declarations.

(defmethod khazern:initial-declarations ((clause for-as-over))
  `((ignorable ,(write-var clause) ,(index-var clause))))

(defmethod khazern:final-declarations ((clause for-as-over))
  (khazern:generate-variable-declarations (var-spec clause) (type-spec clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute prologue-form.

(defmethod khazern:prologue-form ((clause for-as-over) end-tag)
  `(progn
     (multiple-value-setq (,(state-var clause) ,(limit-var clause) ,(from-end-var clause)
                           ,(step-var clause) ,(endp-var clause) ,(read-var clause)
                           ,(write-var clause) ,(index-var clause))
                          (sequence:make-sequence-iterator ,(form-var clause)))
     ,(khazern:termination-form clause end-tag)
     ,(khazern:generate-assignments (var-spec clause)
                                    `(funcall ,(read-var clause) ,(form-var clause)
                                              ,(state-var clause)))
     ,.(when (other-var clause)
         (list (khazern:generate-assignments (other-var clause)
                                             `(funcall ,(index-var clause) ,(form-var clause)
                                                       ,(state-var clause)))))
     (setf ,(state-var clause)
           (funcall ,(step-var clause) ,(form-var clause) ,(state-var clause) ,(from-end-var clause)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute termination-form

(defmethod khazern:termination-form ((clause for-as-over) end-tag)
  `(when (funcall ,(endp-var clause) ,(form-var clause) ,(state-var clause)
                  ,(limit-var clause) ,(from-end-var clause))
     (go ,end-tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute step-form.

(defmethod khazern:step-form ((clause for-as-over))
  `(progn
     ,(khazern:generate-assignments (var-spec clause)
                                    `(funcall ,(read-var clause) ,(form-var clause)
                                              ,(state-var clause)))
     ,.(when (other-var clause)
         (list (khazern:generate-assignments (other-var clause)
                                             `(funcall ,(index-var clause) ,(form-var clause)
                                                       ,(state-var clause)))))
     (setf ,(state-var clause)
           (funcall ,(step-var clause) ,(form-var clause)
                    ,(state-var clause) ,(from-end-var clause)))))
