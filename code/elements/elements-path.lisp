(cl:in-package #:khazern-elements)

#+abcl (require :extensible-sequences)

#-(or abcl clasp sbcl)
(defun make-sequence-iterator (seq &key (start 0) end from-end)
  (let ((end (or end (length seq))))
    (values (if from-end
                (1- end)
                start)
            (if from-end
                (1- start)
                end)
            (and from-end t)
            (lambda (seq iterator from-end)
              (declare (ignore seq))
              (if from-end
                  (1- iterator)
                  (1+ iterator)))
            (lambda (seq iterator limit from-end)
              (declare (ignore seq))
              (= iterator limit))
            (lambda (seq iterator)
              (elt seq iterator))
            nil
            (lambda (seq iterator)
              (declare (ignore seq))
              iterator))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ELEMENTS Path

(defclass for-as-elements (khazern::for-as-subclause)
  ((%var :accessor var
         :initarg :var)
   (%start-var :reader start-var
               :initform (gensym "START"))
   (%start-form :accessor start-form
                :initform 0)
   (%end-var :reader end-var
             :initform (gensym "START"))
   (%end-form :accessor end-form
              :initform nil)
   (%from-end-var :reader from-end-var
               :initform (gensym "START"))
   (%from-end-form :accessor from-end-form
                :initform nil)
   (%in-var :reader in-var
            :initform (gensym "IN"))
   (%in-form :accessor in-form)
   (%index-var :accessor index-var
               :initform nil)
   (%iterator-var :reader iterator-var
                  :initform (gensym "ITERATOR"))
   (%limit-var :reader limit-var
               :initform (gensym "LIMIT"))
   (%step-func :reader step-func
               :initform (gensym "STEP"))
   (%endp-func :reader endp-func
               :initform (gensym "ENDP"))
   (%read-func :reader read-func
               :initform (gensym "READ"))
   (%write-func :reader write-func
                :initform (gensym "WRITE"))
   (%index-func :reader index-func
                :initform (gensym "INDEX"))))

(defmethod khazern:map-variables (function (clause for-as-elements))
  (khazern:map-variables function (khazern:var clause))
  #+(or)(when (index-var clause)
    (funcall function (index-func) 'fixnum nil)))

(defun make-elements-path (name var-spec type-spec)
  (declare (ignore name))
  (make-instance 'for-as-elements
                 :var (make-instance 'khazern:d-spec
                                     :var-spec var-spec
                                     :type-spec type-spec)))

(defmethod khazern:path-preposition-key ((instance for-as-elements) name)
  (cond ((member name '(:in :of) :test #'khazern:symbol-equal)
         :in)
        ((khazern:symbol-equal name :start)
         :start)
        ((khazern:symbol-equal name :end)
         :end)
        ((khazern:symbol-equal name :from-end)
         :from-end)
        ((khazern:symbol-equal name :using)
         :using)
        (t
         nil)))

(defmethod (setf khazern:path-preposition) (expression (instance for-as-elements) (key (eql :in)))
  (setf (in-form instance) expression))

(defmethod (setf khazern:path-preposition) (expression (instance for-as-elements) (key (eql :start)))
  (setf (start-form instance) expression))

(defmethod (setf khazern:path-preposition) (expression (instance for-as-elements) (key (eql :end)))
  (setf (end-form instance) expression))

(defmethod (setf khazern:path-preposition) (expression (instance for-as-elements) (key (eql :from-end)))
  (setf (from-end-form instance) expression))

(defmethod (setf khazern:path-preposition) (expression (instance for-as-elements) (key (eql :using)))
  (setf (index-var instance) (second expression))
  expression)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute bindings.

(defmethod khazern:initial-bindings ((clause for-as-elements))
  `((,(in-var clause) ,(in-form clause))
    (,(start-var clause) ,(start-form clause))
    (,(end-var clause) ,(end-form clause))
    (,(from-end-var clause) ,(from-end-form clause))
    ,(iterator-var clause)
    ,(limit-var clause)
    ,(step-func clause)
    ,(endp-func clause)
    ,(read-func clause)
    ,(write-func clause)
    ,(index-func clause)
    ,@(when (index-var clause)
        `((,(index-var clause) nil)))))

(defmethod khazern:final-bindings ((clause for-as-elements))
  (nconc (khazern:d-spec-outer-bindings (var clause))
         #+(or)(when (other-var clause)
           (khazern:d-spec-outer-bindings (other-var clause)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute declarations.

(defmethod khazern:initial-declarations ((clause for-as-elements))
  `((ignorable ,(write-func clause) ,(index-func clause))))

(defmethod khazern:final-declarations ((clause for-as-elements))
  (khazern:d-spec-outer-declarations (var clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute prologue-forms.

(defmethod khazern:prologue-forms ((clause for-as-elements) end-tag)
  `((multiple-value-setq (,(iterator-var clause) ,(limit-var clause) ,(from-end-var clause)
                          ,(step-func clause) ,(endp-func clause) ,(read-func clause)
                          ,(write-func clause) ,(index-func clause))
      (#+(or abcl clasp sbcl) sequence:make-sequence-iterator
       #-(or abcl clasp sbcl) make-sequence-iterator
         ,(in-var clause)
         :start ,(start-var clause)
         :end ,(end-var clause)
         :from-end ,(from-end-var clause)))
    (when (funcall ,(endp-func clause) ,(in-var clause) ,(iterator-var clause)
                   ,(limit-var clause) ,(from-end-var clause))
      (go ,end-tag))
    ,@(khazern:d-spec-inner-form (var clause)
                                 `(funcall ,(read-func clause) ,(in-var clause)
                                           ,(iterator-var clause)))
    ,@(when (index-var clause)
        `((setq ,(index-var clause)
	        (funcall ,(index-func clause) ,(in-var clause)
			 ,(iterator-var clause)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute termination-forms

(defmethod khazern:termination-forms ((clause for-as-elements) end-tag)
  `((setq ,(iterator-var clause)
          (funcall ,(step-func clause) ,(in-var clause)
                   ,(iterator-var clause) ,(from-end-var clause)))
    (when (funcall ,(endp-func clause) ,(in-var clause) ,(iterator-var clause)
                   ,(limit-var clause) ,(from-end-var clause))
      (go ,end-tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute step-forms.

(defmethod khazern:step-forms ((clause for-as-elements))
  (nconc (khazern:d-spec-inner-form (var clause)
                                    `(funcall ,(read-func clause) ,(in-var clause)
                                              ,(iterator-var clause)))
         (when (index-var clause)
           `((setq ,(index-var clause)
	           (funcall ,(index-func clause) ,(in-var clause)
			    ,(iterator-var clause)))))))
  
  
