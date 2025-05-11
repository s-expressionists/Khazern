(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-HASH

(defclass for-as-hash (for-as-subclause)
  ((%hash-table-form :accessor hash-table-form
                     :initarg :hash-table-form)
   (%hash-table-var :reader hash-table-var
                    :initform (gensym))
   (%temp-entry-p-var :reader temp-entry-p-var
                      :initform (gensym))
   (%temp-key-var :reader temp-key-var
                  :initform (gensym))
   (%temp-value-var :reader temp-value-var
                    :initform (gensym))
   (%iterator-var :reader iterator-var
                  :initform (gensym))
   (%other-var :accessor other-var
               :initarg :other-var
               :initform (make-instance 'd-spec
                                        :var-spec nil))))

(defclass for-as-hash-key (for-as-hash) ())

(defclass for-as-hash-value (for-as-hash) ())

(defmethod map-variables (function (clause for-as-hash))
  (map-variables function (var clause))
  (map-variables function (other-var clause)))

(defun make-for-as-hash-key (name var-spec type-spec)
  (declare (ignore name))
  (make-instance 'for-as-hash-key
                 :var (make-instance 'd-spec
                                     :var-spec var-spec
                                     :type-spec type-spec)))

(defun make-for-as-hash-value (name var-spec type-spec)
  (declare (ignore name))
  (make-instance 'for-as-hash-value
                 :var (make-instance 'd-spec
                                     :var-spec var-spec
                                     :type-spec type-spec)))

(defmethod path-preposition-key ((instance for-as-hash) name)
  (cond ((member name '(:in :of) :test #'symbol-equal)
         :in)
        ((symbol-equal name :using)
         :using)
        (t
         nil)))

(defmethod (setf path-preposition) ((expression cons) (instance for-as-hash-key) (key (eql :using)))
  (if (and (= (length expression) 2)
           (not (member (first expression) '(:hash-value :hash-values) :test #'symbol-equal)))
      (error "Unknown USING ~a" (first expression))
      (call-next-method)))

(defmethod (setf path-preposition) ((expression cons) (instance for-as-hash-value) (key (eql :using)))
  (if (and (= (length expression) 2)
           (not (member (first expression) '(:hash-key :hash-keys) :test #'symbol-equal)))
      (error "Unknown USING ~a" (first expression))
      (call-next-method)))

(defmethod (setf path-preposition) (expression (instance for-as-hash) (key (eql :in)))
  (setf (hash-table-form instance) expression))

(defmethod (setf path-preposition) (expression (instance for-as-hash) (key (eql :using)))
  (setf (other-var instance) (make-instance 'd-spec
                                            :var-spec (second expression)))
  expression)

(defmethod analyze ((clause for-as-hash))
  (unless (slot-boundp clause '%hash-table-form)
    (error "Missing IN/OF preposition")))

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

(defmethod prologue-forms ((subclause for-as-hash-key))
  `((multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-key-var subclause)
                          ,(temp-value-var subclause))
      (,(iterator-var subclause)))
    (unless ,(temp-entry-p-var subclause)
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var subclause)
                         (temp-key-var subclause))
    ,@(d-spec-inner-form (other-var subclause)
                         (temp-value-var subclause))))

(defmethod prologue-forms ((subclause for-as-hash-value))
  `((multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-key-var subclause)
                          ,(temp-value-var subclause))
      (,(iterator-var subclause)))
    (unless ,(temp-entry-p-var subclause)
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var subclause)
                         (temp-value-var subclause))
    ,@(d-spec-inner-form (other-var subclause)
                         (temp-key-var subclause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the termination form.

(defmethod termination-forms ((subclause for-as-hash))
  `((multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-key-var subclause)
                          ,(temp-value-var subclause))
      (,(iterator-var subclause)))
    (unless ,(temp-entry-p-var subclause)
      (go ,*epilogue-tag*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step form.

(defmethod step-forms ((subclause for-as-hash-key))
  `(,@(d-spec-inner-form (var subclause)
                         (temp-key-var subclause))
    ,@(d-spec-inner-form (other-var subclause)
                         (temp-value-var subclause))))

(defmethod step-forms ((subclause for-as-hash-value))
  `(,@(d-spec-inner-form (var subclause)
                         (temp-value-var subclause))
    ,@(d-spec-inner-form (other-var subclause)
                         (temp-key-var subclause))))
