(cl:in-package #:khazern-extension)

#+abcl (require :extensible-sequences)

(defclass for-as-elements (khazern:for-as-iteration-path)
  ((%in-ref :accessor in-ref
            :initform nil)
   (%start-ref :accessor start-ref
               :initform 0)
   (%end-ref :accessor end-ref
             :initform nil)
   (%from-end-ref :accessor from-end-ref
               :initform nil)
   (%index-var :accessor index-var
               :initform nil)
   #+(or abcl clasp sbcl)
   (%from-end-var :reader from-end-var
                  :initform (gensym "FROM-END-"))
   (%iterator-var :reader iterator-var
                  :initform (gensym "ITERATOR"))
   #-(or abcl clasp sbcl)
   (%step-var :reader step-var
              :initform (gensym "STEP"))
   (%limit-var :reader limit-var
               :initform (gensym "LIMIT"))
   #+(or abcl clasp sbcl)
   (%step-func :reader step-func
               :initform (gensym "STEP"))
   #+(or abcl clasp sbcl)
   (%endp-func :reader endp-func
               :initform (gensym "ENDP"))
   #+(or abcl clasp sbcl)
   (%read-func :reader read-func
               :initform (gensym "READ"))
   #+(or abcl clasp sbcl)
   (%write-func :reader write-func
                :initform (gensym "WRITE"))
   #+(or abcl clasp sbcl)
   (%index-func :reader index-func
                :initform (gensym "INDEX")))
  (:default-initargs :preposition-names (list :in :of :start :end :from-end)
                     :using-names (list :index)))

(defmethod (setf khazern:iteration-path-preposition)
    :after (value (instance for-as-elements) key)
  (declare (ignore value))
  (setf (khazern:iteration-path-preposition-names instance)
        (delete-if (lambda (name)
                     (or (eq name key)
                         (and (eq key :in)
                              (eq name :of))
                         (and (eq key :of)
                              (eq name :in))))
                   (khazern:iteration-path-preposition-names instance))))

(defmethod (setf khazern:iteration-path-preposition)
    (value (instance for-as-elements) (key (eql :in)))
  (setf (in-ref instance) (khazern:add-simple-binding instance :var "IN" :form value
                                                               :type 'sequence))
  value)

(defmethod (setf khazern:iteration-path-preposition)
    (value (instance for-as-elements) (key (eql :of)))
  (setf (in-ref instance) (khazern:add-simple-binding instance :var "IN" :form value
                                                               :type 'sequence))
  value)

(defmethod (setf khazern:iteration-path-preposition)
    (value (instance for-as-elements) (key (eql :start)))
  (setf (start-ref instance) (khazern:add-simple-binding instance :var "START" :form value
                                                                  :fold t :type 'fixnum))
  value)

(defmethod (setf khazern:iteration-path-preposition)
    (value (instance for-as-elements) (key (eql :end)))
  (setf (end-ref instance) (khazern:add-simple-binding instance :var "END" :form value
                                                                :fold t :type 'fixnum))
  value)

(defmethod (setf khazern:iteration-path-preposition)
    (value (instance for-as-elements) (key (eql :from-end)))
  (setf (from-end-ref instance) (khazern:add-simple-binding instance :var "FROM-END-"
                                                                     :form value :fold t))
  value)

(defmethod (setf khazern:iteration-path-using) :after
    (expression (instance for-as-elements) key)
  (setf (khazern:iteration-path-using-names instance)
        (delete key (khazern:iteration-path-using-names instance))))

(defmethod (setf khazern:iteration-path-using)
    (value (instance for-as-elements) (key (eql :index)))
  (setf (index-var instance) value))

(defmethod khazern:analyze ((instance for-as-elements))
  (when (eq (khazern::type-spec (khazern:var instance)) khazern::*placeholder-result*)
    (setf (khazern::type-spec (khazern:var instance)) t)))

(defmethod khazern:bindings nconc ((clause for-as-elements))
  (nconc (list (iterator-var clause)
               (limit-var clause))
         #+(or abcl clasp sbcl)
         (list (step-func clause)
               (endp-func clause)
               (read-func clause)
               (write-func clause)
               (index-func clause))
         #-(or abcl clasp sbcl)
         (list (step-var clause))
         (when (index-var clause)
           `((,(index-var clause) nil)))))

(defmethod khazern:declarations nconc ((clause for-as-elements))
  #+(or abcl clasp sbcl)
  `((ignorable ,(write-func clause) ,(index-func clause))))

(defmethod khazern:begin-step-forms ((clause for-as-elements) initialp)
  #+(or abcl clasp sbcl)
  (nconc (if initialp
             `((multiple-value-setq (,(iterator-var clause) ,(limit-var clause)
                                     ,(from-end-var clause) ,(step-func clause)
                                     ,(endp-func clause) ,(read-func clause)
                                     ,(write-func clause) ,(index-func clause))
                 (sequence:make-sequence-iterator ,(in-ref clause)
                                                  :start ,(start-ref clause)
                                                  :end ,(end-ref clause)
                                                  :from-end ,(from-end-ref clause))))
             `((setq ,(iterator-var clause)
                     (funcall ,(step-func clause) ,(in-ref clause)
                              ,(iterator-var clause) ,(from-end-var clause)))))
         `((when (funcall ,(endp-func clause) ,(in-ref clause) ,(iterator-var clause)
                          ,(limit-var clause) ,(from-end-var clause))
             (go ,khazern:*epilogue-tag*)))
         (khazern::d-spec-prep-assignments (khazern:var clause)
                                           `(funcall ,(read-func clause) ,(in-ref clause)
                                                     ,(iterator-var clause))))
  #-(or abcl clasp sbcl)
  (nconc (if initialp
             `((let ((end (or ,(end-ref clause) (length ,(in-ref clause)))))
                 (setq ,(iterator-var clause) (if ,(from-end-ref clause)
                                                  (1- end)
                                                  ,(start-ref clause))
                       ,(limit-var clause) (if ,(from-end-ref clause)
                                               (1- ,(start-ref clause))
                                               end)
                       ,(step-var clause) (if ,(from-end-ref clause) -1 1))))
             `((incf ,(iterator-var clause) ,(step-var clause))))
         `((when (= ,(iterator-var clause) ,(limit-var clause))
             (go ,khazern:*epilogue-tag*)))
         (khazern::d-spec-prep-assignments (khazern:var clause)
                                           `(elt ,(in-ref clause) ,(iterator-var clause)))))

(defmethod khazern:finish-step-forms ((clause for-as-elements) initialp)
  (declare (ignore initialp))
  #+(or abcl clasp sbcl)
  (nconc (khazern::d-spec-inner-assignments (khazern:var clause)
                                            `(funcall ,(read-func clause) ,(in-ref clause)
                                                      ,(iterator-var clause)))
         (when (index-var clause)
           `((setq ,(index-var clause)
	           (funcall ,(index-func clause) ,(in-ref clause)
			    ,(iterator-var clause))))))
    #-(or abcl clasp sbcl)
  (nconc (khazern::d-spec-inner-assignments (khazern:var clause)
                                            `(elt ,(in-ref clause) ,(iterator-var clause)))
         (when (index-var clause)
           `((setq ,(index-var clause) ,(iterator-var clause))))))
