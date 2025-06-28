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
   (%index-ref :accessor index-ref
               :initform nil)
   (%iterator-ref :accessor iterator-ref)
   #-(or abcl clasp sbcl)
   (%step-ref :accessor step-ref)
   (%limit-ref :accessor limit-ref)
   #+(or abcl clasp sbcl)
   (%step-func :accessor step-func)
   #+(or abcl clasp sbcl)
   (%endp-func :accessor endp-func)
   #+(or abcl clasp sbcl)
   (%read-func :accessor read-func)
   #+(or abcl clasp sbcl)
   (%write-func :accessor write-func)
   #+(or abcl clasp sbcl)
   (%index-func :accessor index-func))
  (:default-initargs :preposition-names (list '(:in :of) :start :end :from-end)
                     :using-names (list :index)))

(defmethod initialize-instance :after ((instance for-as-elements) &rest initargs &key)
  (declare (ignore initargs))
  (setf (limit-ref instance) (khazern:add-simple-binding instance :var "LIMIT")
        (iterator-ref instance) (khazern:add-simple-binding instance :var "ITER"))
  #+(or abcl clasp sbcl)
  (setf (step-func instance) (khazern:add-simple-binding instance :var "STEP")
        (endp-func instance) (khazern:add-simple-binding instance :var "ENDP")
        (read-func instance) (khazern:add-simple-binding instance :var "READ")
        (write-func instance) (khazern:add-simple-binding instance :var "WRITE" :ignorable t)
        (index-func instance) (khazern:add-simple-binding instance :var "INDEX" :ignorable t))
  #-(or abcl clasp sbcl)
  (setf (step-ref instance) (khazern:add-simple-binding instance :var "STEP" :type 'fixnum)))

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
                                                                     :form value))
  value)

(defmethod (setf khazern:iteration-path-using)
    (value (instance for-as-elements) (key (eql :index)))
  (setf (index-ref instance) (khazern:add-simple-binding instance :var value
                                                                  :type 'fixnum
                                                                  :form 0)))

(defmethod khazern:analyze ((instance for-as-elements))
  (when (eq (khazern:type-spec (khazern:var instance)) khazern:*placeholder-result*)
    (setf (khazern:type-spec (khazern:var instance)) t))
  (unless (from-end-ref instance)
    (setf (from-end-ref instance) (khazern:add-simple-binding instance :var "FROM-END-"
                                                                       :form nil))))

(defmethod khazern:begin-step-forms ((clause for-as-elements) initialp)
  #+(or abcl clasp sbcl)
  (nconc (if initialp
             `((multiple-value-setq (,(iterator-ref clause) ,(limit-ref clause)
                                     ,(from-end-ref clause) ,(step-func clause)
                                     ,(endp-func clause) ,(read-func clause)
                                     ,(write-func clause) ,(index-func clause))
                 (sequence:make-sequence-iterator ,(in-ref clause)
                                                  :start ,(start-ref clause)
                                                  :end ,(end-ref clause)
                                                  :from-end ,(from-end-ref clause))))
             `((setq ,(iterator-ref clause)
                     (funcall ,(step-func clause) ,(in-ref clause)
                              ,(iterator-ref clause) ,(from-end-ref clause)))))
         `((when (funcall ,(endp-func clause) ,(in-ref clause) ,(iterator-ref clause)
                          ,(limit-ref clause) ,(from-end-ref clause))
             (go ,khazern:*epilogue-tag*))))
  #-(or abcl clasp sbcl)
  (nconc (if initialp
             `((let ((end (or ,(end-ref clause) (length ,(in-ref clause)))))
                 (setq ,(iterator-ref clause) (if ,(from-end-ref clause)
                                                  (1- end)
                                                  ,(start-ref clause))
                       ,(limit-ref clause) (if ,(from-end-ref clause)
                                               (1- ,(start-ref clause))
                                               end)
                       ,(step-ref clause) (if ,(from-end-ref clause) -1 1))))
             `((incf ,(iterator-ref clause) ,(step-ref clause))))
         `((when (= ,(iterator-ref clause) ,(limit-ref clause))
             (go ,khazern:*epilogue-tag*)))))

(defmethod khazern:finish-step-forms ((clause for-as-elements) initialp)
  (declare (ignore initialp))
  #+(or abcl clasp sbcl)
  (nconc (khazern:destructuring-set (khazern:var clause)
                                    `(funcall ,(read-func clause) ,(in-ref clause)
                                              ,(iterator-ref clause)))
         (when (index-ref clause)
           `((setq ,(index-ref clause)
	           (funcall ,(index-func clause) ,(in-ref clause)
			    ,(iterator-ref clause))))))
  #-(or abcl clasp sbcl)
  (nconc (khazern:destructuring-set (khazern:var clause)
                                    `(elt ,(in-ref clause) ,(iterator-ref clause)))
         (when (index-ref clause)
           `((setq ,(index-ref clause) ,(iterator-ref clause))))))
