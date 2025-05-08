(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-HASH

(defclass for-as-hash (for-as-subclause)
  ((%hash-table-form :initarg :hash-table-form :reader hash-table-form)
   (%hash-table-var :initform (gensym) :reader hash-table-var)
   (%temp-entry-p-var :initform (gensym) :reader temp-entry-p-var)
   (%temp-key-var :initform (gensym) :reader temp-key-var)
   (%temp-value-var :initform (gensym) :reader temp-value-var)
   (%iterator-var :initform (gensym) :reader iterator-var)
   (%other-var :initarg :other-var :reader other-var)))

(defclass for-as-hash-key (for-as-hash) ())

(defclass for-as-hash-value (for-as-hash) ())

(defmethod map-variables (function (clause for-as-hash))
  (map-variables function (var clause))
  (map-variables function (other-var clause)))

(defun make-for-as-hash-key (name var-spec type-spec data
                             &key (in nil inp) using)
  (declare (ignore name data))
  (unless inp
    (error "IN preposition is required."))
  (when (> (length using) 2)
    (error "Multiple USING key values."))
  (unless (or (null using)
              (symbol-equal (first using) :hash-value)
              (symbol-equal (first using) :hash-values))
    (error "Unknown USING ~a" (first using)))
  (make-instance 'for-as-hash-key
                 :var (make-instance 'd-spec
                                     :var-spec var-spec
                                     :type-spec type-spec)
                 :hash-table-form in
                 :other-var (make-instance 'd-spec
                                           :var-spec (second using))))


(defun make-for-as-hash-value (name var-spec type-spec data
                               &key (in nil inp) using)
  (declare (ignore name data))
  (unless inp
    (error "IN preposition is required."))
  (when (> (length using) 2)
    (error "Multiple USING key values."))
  (unless (or (null using)
              (symbol-equal (first using) :hash-key)
              (symbol-equal (first using) :hash-keys))
    (error "Unknown USING ~a" (first using)))
  (make-instance 'for-as-hash-value
                 :var (make-instance 'd-spec
                                     :var-spec var-spec
                                     :type-spec type-spec)
                 :hash-table-form in
                 :other-var (make-instance 'd-spec
                                           :var-spec (second using))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the initial bindings.

(defmethod initial-bindings ((clause for-as-hash))
  `((,(hash-table-var clause) ,(hash-table-form clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the subclause wrapper.

(defmethod wrap-subclause ((subclause for-as-hash) inner-form)
  (wrap-let `((,(temp-entry-p-var subclause) nil)
              (,(temp-key-var subclause) nil)
              (,(temp-value-var subclause) nil)
              ,.(d-spec-outer-bindings (var subclause))
              ,.(d-spec-outer-bindings (other-var subclause)))
            (d-spec-outer-declarations (var subclause))
            `((with-hash-table-iterator
                  (,(iterator-var subclause) ,(hash-table-var subclause))
                ,@inner-form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue form.

(defmethod prologue-forms ((subclause for-as-hash-key) end-tag)
  `((multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-key-var subclause)
                          ,(temp-value-var subclause))
      (,(iterator-var subclause)))
    (unless ,(temp-entry-p-var subclause)
      (go ,end-tag))
    ,@(d-spec-inner-form (var subclause)
                         (temp-key-var subclause))
    ,@(d-spec-inner-form (other-var subclause)
                         (temp-value-var subclause))
    (multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-key-var subclause)
                          ,(temp-value-var subclause))
      (,(iterator-var subclause)))))

(defmethod prologue-forms ((subclause for-as-hash-value) end-tag)
  `((multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-key-var subclause)
                          ,(temp-value-var subclause))
      (,(iterator-var subclause)))
    (unless ,(temp-entry-p-var subclause)
      (go ,end-tag))
    ,@(d-spec-inner-form (var subclause)
                         (temp-value-var subclause))
    ,@(d-spec-inner-form (other-var subclause)
                         (temp-key-var subclause))
    (multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-key-var subclause)
                          ,(temp-value-var subclause))
      (,(iterator-var subclause)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the termination form.

(defmethod termination-forms ((subclause for-as-hash) end-tag)
  `((unless ,(temp-entry-p-var subclause)
      (go ,end-tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step form.

(defmethod step-forms ((subclause for-as-hash-key))
  `(,@(d-spec-inner-form (var subclause)
                         (temp-key-var subclause))
    ,@(d-spec-inner-form (other-var subclause)
                         (temp-value-var subclause))
    (multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-key-var subclause)
                          ,(temp-value-var subclause))
      (,(iterator-var subclause)))))

(defmethod step-forms ((subclause for-as-hash-value))
  `(,@(d-spec-inner-form (var subclause)
                         (temp-value-var subclause))
    ,@(d-spec-inner-form (other-var subclause)
                         (temp-key-var subclause))
    (multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-key-var subclause)
                          ,(temp-value-var subclause))
      (,(iterator-var subclause)))))
