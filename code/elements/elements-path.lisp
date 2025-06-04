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

(defclass for-as-elements ()
  ((%var :accessor var
         :accessor khazern:var
         :initarg :var)
   (%preposition-names :accessor khazern:iteration-path-preposition-names
                       :initform (list :in :of :start :end :from-end))
   (%using-names :accessor khazern:iteration-path-using-names
                 :initform (list :index))
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
  (khazern:map-variables function (khazern:var clause)))

(defmethod (setf khazern:iteration-path-preposition) :after (expression (instance for-as-elements) key)
  (setf (khazern:iteration-path-preposition-names instance)
        (delete-if (lambda (name)
                     (or (eq name key)
                         (and (eq key :in)
                              (eq name :of))
                         (and (eq key :of)
                              (eq name :in))))
                   (khazern:iteration-path-preposition-names instance))))

(defmethod (setf khazern:iteration-path-preposition) (expression (instance for-as-elements) (key (eql :in)))
  (setf (in-form instance) expression))

(defmethod (setf khazern:iteration-path-preposition) (expression (instance for-as-elements) (key (eql :of)))
  (setf (in-form instance) expression))

(defmethod (setf khazern:iteration-path-preposition) (expression (instance for-as-elements) (key (eql :start)))
  (setf (start-form instance) expression))

(defmethod (setf khazern:iteration-path-preposition) (expression (instance for-as-elements) (key (eql :end)))
  (setf (end-form instance) expression))

(defmethod (setf khazern:iteration-path-preposition) (expression (instance for-as-elements) (key (eql :from-end)))
  (setf (from-end-form instance) expression))

(defmethod (setf khazern:iteration-path-using) :after (expression (instance for-as-elements) key)
  (setf (khazern:iteration-path-using-names instance)
        (delete key (khazern:iteration-path-using-names instance))))

(defmethod (setf khazern:iteration-path-using) (value (instance for-as-elements) (key (eql :index)))
  (setf (index-var instance) value))

(defmethod khazern:analyze ((instance for-as-elements))
  (when (eq (khazern::type-spec (var instance)) khazern::*placeholder-result*)
    (setf (khazern::type-spec (var instance)) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute bindings.

(defmethod khazern:initial-bindings ((clause for-as-elements))
  (nconc `((,(in-var clause) ,(in-form clause))
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
               `((,(index-var clause) nil))))
         (khazern:d-spec-outer-bindings (var clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute declarations.

(defmethod khazern:initial-declarations ((clause for-as-elements))
  (list* `(ignorable ,(write-func clause) ,(index-func clause))
         (khazern:d-spec-outer-declarations (var clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute initial step forms.

(defmethod khazern:initial-step-forms ((clause for-as-elements))
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
      (go ,khazern:*epilogue-tag*))
    ,@(khazern:d-spec-inner-form (var clause)
                                 `(funcall ,(read-func clause) ,(in-var clause)
                                           ,(iterator-var clause)))
    ,@(when (index-var clause)
        `((setq ,(index-var clause)
	        (funcall ,(index-func clause) ,(in-var clause)
			 ,(iterator-var clause)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the subsequent step forms.

(defmethod khazern:subsequent-step-forms ((clause for-as-elements))
  (nconc  `((setq ,(iterator-var clause)
                  (funcall ,(step-func clause) ,(in-var clause)
                           ,(iterator-var clause) ,(from-end-var clause)))
            (when (funcall ,(endp-func clause) ,(in-var clause) ,(iterator-var clause)
                           ,(limit-var clause) ,(from-end-var clause))
              (go ,khazern:*epilogue-tag*)))
          (khazern:d-spec-inner-form (var clause)
                                     `(funcall ,(read-func clause) ,(in-var clause)
                                               ,(iterator-var clause)))
          (when (index-var clause)
            `((setq ,(index-var clause)
	            (funcall ,(index-func clause) ,(in-var clause)
			     ,(iterator-var clause)))))))


