(cl:in-package #:khazern-extension)

#+abcl (require :extensible-sequences)

(defclass for-as-elements (khazern:clause)
  ((%var :accessor var
         :initarg :var)
   (%in-ref :accessor in-ref
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
   (%limit-ref :accessor limit-ref)
   (%step-func :accessor step-func)
   (%endp-func :accessor endp-func)
   (%read-func :accessor read-func)
   (%write-func :accessor write-func)
   (%index-func :accessor index-func)))

(defmethod initialize-instance :after ((instance for-as-elements) &rest initargs &key)
  (declare (ignore initargs))
  (khazern:add-binding instance (var instance))
  (setf (limit-ref instance) (khazern:add-simple-binding instance :var "LIMIT")
        (iterator-ref instance) (khazern:add-simple-binding instance :var "ITER")
        (step-func instance) (khazern:add-simple-binding instance :var "STEP")
        (endp-func instance) (khazern:add-simple-binding instance :var "ENDP")
        (read-func instance) (khazern:add-simple-binding instance :var "READ")
        (write-func instance) (khazern:add-simple-binding instance :var "WRITE" :ignorable t)
        (index-func instance) (khazern:add-simple-binding instance :var "INDEX" :ignorable t)))

(defmethod khazern:preposition-names ((client extension-client) (instance for-as-elements))
  (values '((:in :of) :start :end :from-end)
          '((:in :of))
          '(:index :indicies)))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance for-as-elements) (key (eql :in)))
  (setf (in-ref instance) (khazern:add-simple-binding instance
                                                      :var "IN"
                                                      :form (khazern:parse-token)
                                                      :type 'sequence)))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance for-as-elements) (key (eql :of)))
  (setf (in-ref instance) (khazern:add-simple-binding instance
                                                      :var "IN"
                                                      :form (khazern:parse-token)
                                                      :type '(or array sequence))))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance for-as-elements) (key (eql :start)))
  (setf (start-ref instance) (khazern:add-simple-binding instance
                                                         :var "START"
                                                         :form (khazern:parse-token)
                                                         :fold t :type 'fixnum)))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance for-as-elements) (key (eql :end)))
  (setf (end-ref instance) (khazern:add-simple-binding instance
                                                       :var "END" :form (khazern:parse-token)
                                                       :fold t :type 'fixnum)))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance for-as-elements) (key (eql :from-end)))
  (setf (from-end-ref instance) (khazern:add-simple-binding instance
                                                            :var "FROM-END-"
                                                            :form (khazern:parse-token))))

(defun parse-index (instance)
  (let ((var (khazern:parse-d-spec :type-spec khazern:*placeholder-result* :ignorable t)))
    (when (eq (khazern:type-spec var) khazern:*placeholder-result*)
      (setf (khazern:type-spec var)
            (if (listp (khazern:var-spec var))
                'fixnum
                '(or fixnum list))))
    (setf (index-ref instance) (khazern:add-binding instance var))))

(defmethod khazern:parse-using
    ((client extension-client) (instance for-as-elements) (key (eql :index)))
  (parse-index instance))
  
(defmethod khazern:parse-using
    ((client extension-client) (instance for-as-elements) (key (eql :indicies)))
  (parse-index instance))
  
(defmethod khazern:analyze ((client extension-client) (instance for-as-elements))
  (when (eq (khazern:type-spec (var instance)) khazern:*placeholder-result*)
    (setf (khazern:type-spec (var instance)) t))
  (unless (from-end-ref instance)
    (setf (from-end-ref instance) (khazern:add-simple-binding instance :var "FROM-END-"
                                                              :form nil))))

(defmethod khazern:step-intro-forms ((clause for-as-elements) initialp)
  (with-accessors ((iterator-ref iterator-ref)
                   (limit-ref limit-ref)
                   (start-ref start-ref)
                   (end-ref end-ref)
                   (from-end-ref from-end-ref)
                   (step-func step-func)
                   (endp-func endp-func)
                   (read-func read-func)
                   (write-func write-func)
                   (index-func index-func)
                   (in-ref in-ref))
      clause
    (nconc (if initialp
               `((multiple-value-setq (,iterator-ref ,limit-ref ,from-end-ref ,step-func
                                       ,endp-func ,read-func ,write-func ,index-func)
                   (cond ((null ,in-ref)
                          (go ,khazern:*epilogue-tag*))
                         ((and (arrayp ,in-ref) (not (vectorp ,in-ref)))
                          (let ((start (typecase ,start-ref
                                         (null
                                          0)
                                         (list
                                          (apply #'array-row-major-index
                                                 (the array ,in-ref) ,start-ref))
                                         (otherwise
                                          ,start-ref)))
                                (end (typecase ,end-ref
                                       (null
                                        (array-total-size (the array ,in-ref)))
                                       (list
                                        (apply #'array-row-major-index
                                               (the array ,in-ref) ,end-ref))
                                       (otherwise
                                        ,end-ref))))
                            (values (if ,from-end-ref
                                        (1- end)
                                        start)
                                    (if ,from-end-ref
                                        start
                                        end)
                                    ,from-end-ref
                                    (lambda (seq iter from-end-p)
                                      (declare (ignore seq))
                                      (if from-end-p
                                          (1- iter)
                                          (1+ iter)))
                                    (lambda (seq iter limit from-end-p)
                                      (declare (ignore seq))
                                      (if from-end-p
                                          (< iter limit)
                                          (>= iter limit)))
                                    #'row-major-aref
                                    (lambda (element seq iter)
                                      (setf (row-major-aref (the array seq) iter) element))
                                    (lambda (array row-major-index)
                                      (maplist (lambda (x)
                                                 (multiple-value-bind (q r)
                                                     (floor row-major-index (apply '* (cdr x)))
                                                   (setf row-major-index r)
                                                   q))
                                               (array-dimensions array))))))
                         #-(or abcl clasp sbcl)
                         ((and (consp ,in-ref) (not ,from-end-ref))
                          (let ((index (or ,start-ref 0)))
                            (values (nthcdr index (the cons ,in-ref))
                                    (when ,end-ref
                                      (nthcdr ,end-ref (the cons ,in-ref)))
                                    nil
                                    (lambda (seq iter from-end-p)
                                      (declare (ignore seq from-end-p))
                                      (incf index)
                                      (cdr iter))
                                    (lambda (seq iter limit from-end-p)
                                      (declare (ignore seq from-end-p))
                                      (eq iter limit))
                                    (lambda (seq iter)
                                      (declare (ignore seq))
                                      (car iter))
                                    (lambda (element seq iter)
                                      (declare (ignore seq))
                                      (rplaca iter element))
                                    (lambda (seq iter)
                                      (declare (ignore seq iter))
                                      index))))
                         (t
                          #+(or abcl clasp sbcl)
                          (sequence:make-sequence-iterator ,in-ref
                                                           :start ,start-ref
                                                           :end ,end-ref
                                                           :from-end ,from-end-ref)
                          #-(or abcl clasp sbcl)
                          (values (if ,from-end-ref
                                      (1- (or ,end-ref
                                              (length ,in-ref)))
                                      ,start-ref)
                                  (if ,from-end-ref
                                      ,start-ref
                                      (or ,end-ref
                                          (length ,in-ref)))
                                  ,from-end-ref
                                  (lambda (seq iter from-end-p)
                                    (declare (ignore seq))
                                    (if from-end-p
                                        (1- iter)
                                        (1+ iter)))
                                  (lambda (seq iter limit from-end-p)
                                    (declare (ignore seq))
                                    (if from-end-p
                                        (< iter limit)
                                        (>= iter limit)))
                                  #'elt
                                  (lambda (element seq iter)
                                    (setf (elt seq iter) element))
                                  (lambda (seq iter)
                                    (declare (ignore seq))
                                    iter))))))
               `((setq ,iterator-ref
                       (funcall ,step-func ,in-ref
                                ,iterator-ref ,from-end-ref))))
           `((when (funcall ,endp-func ,in-ref ,iterator-ref
                            ,limit-ref ,from-end-ref)
               (go ,khazern:*epilogue-tag*))))))

(defmethod khazern:step-outro-forms ((clause for-as-elements) initialp)
  (declare (ignore initialp))
  (nconc (khazern:destructuring-set (var clause)
                                    `(funcall ,(read-func clause) ,(in-ref clause)
                                              ,(iterator-ref clause)))
         (when (index-ref clause)
           (khazern:destructuring-set (index-ref clause)
                                      `(funcall ,(index-func clause) ,(in-ref clause)
		                                ,(iterator-ref clause))))))

(defmethod khazern:make-iteration-path
    ((client extension-client) (name (eql :element)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-elements :var var)))

(defmethod khazern:make-iteration-path
    ((client extension-client) (name (eql :elements)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-elements :var var)))
