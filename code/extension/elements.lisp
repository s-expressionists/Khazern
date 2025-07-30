(cl:in-package #:khazern-extension)

#+abcl (require :extensible-sequences)

(defclass for-as-elements (khazern:clause)
  ((%var :accessor var
         :initarg :var)
   (%in-ref :accessor in-ref
            :initform nil)
   (%start-ref :accessor start-ref
               :initform nil)
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
          '(:index :indices)))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance for-as-elements) (key (eql :in)))
  (setf (in-ref instance) (khazern:add-simple-binding instance
                                                      :var "IN"
                                                      :form (khazern:parse-token)
                                                      :type '(or array sequence))))

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
    ((client extension-client) (instance for-as-elements) (key (eql :indices)))
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
                         ((typep ,in-ref 'sequence)
                          #+(or abcl clasp sbcl)
                          (sequence:make-sequence-iterator ,in-ref
                                                           :start (or ,start-ref 0)
                                                           :end ,end-ref
                                                           :from-end ,from-end-ref)
                          #-(or abcl clasp sbcl)
                          (values (if ,from-end-ref
                                      (1- (or ,end-ref
                                              (length ,in-ref)))
                                      (or ,start-ref 0))
                                  (if ,from-end-ref
                                      (or ,start-ref 0)
                                      (or ,end-ref
                                          (length ,in-ref)))
                                  ,from-end-ref
                                  (if ,from-end-ref
                                      (lambda (seq iter from-end-p)
                                        (declare (ignore seq from-end-p))
                                        (1- iter))
                                      (lambda (seq iter from-end-p)
                                        (declare (ignore seq from-end-p))
                                        (1+ iter)))
                                  (if ,from-end-ref
                                      (lambda (seq iter limit from-end-p)
                                        (declare (ignore seq from-end-p))
                                        (< iter limit))
                                      (lambda (seq iter limit from-end-p)
                                        (declare (ignore seq from-end-p))
                                        (>= iter limit)))
                                  #'elt
                                  (lambda (element seq iter)
                                    (setf (elt seq iter) element))
                                  (lambda (seq iter)
                                    (declare (ignore seq))
                                    iter)))
                         ((not (arrayp ,in-ref))
                          (error 'type-error
                                 :datum ,in-ref :expected-type '(or sequence array)))
                         ((or ,start-ref ,end-ref)
                          (let* ((start (or ,start-ref
                                            (make-array (array-rank (the array ,in-ref))
                                                        :initial-element 0)))
                                 (end (or ,end-ref
                                          (array-dimensions (the array ,in-ref)))))
                            (values (if ,from-end-ref
                                        (mapcar #'1- end)
                                        (copy-list start))
                                    (if ,from-end-ref
                                        start
                                        end)
                                    ,from-end-ref
                                    (if ,from-end-ref
                                        (lambda (seq iter from-end-p)
                                          (declare (ignore from-end-p))
                                          (prog ((pos (1- (array-rank (the array seq)))))
                                           next
                                             (cond ((minusp pos)
                                                    (mapl (lambda (i l)
                                                            (rplaca i (1- (car l))))
                                                          iter start))
                                                   ((eql (elt start pos) (elt iter pos))
                                                    (setf (elt iter pos) (1- (elt end pos)))
                                                    (decf pos)
                                                    (go next))
                                                   (t
                                                    (decf (elt iter pos)))))
                                          iter)
                                        (lambda (seq iter from-end-p)
                                          (declare (ignore from-end-p))
                                          (prog ((pos (1- (array-rank (the array seq)))))
                                           next
                                             (cond ((minusp pos)
                                                    (mapl (lambda (i l)
                                                            (rplaca i (car l)))
                                                          iter end))
                                                   ((eql (elt end pos) (1+ (elt iter pos)))
                                                    (setf (elt iter pos) (elt start pos))
                                                    (decf pos)
                                                    (go next))
                                                   (t
                                                    (incf (elt iter pos)))))
                                          iter))
                                    (if ,from-end-ref
                                        (lambda (seq iter limit from-end-p)
                                          (declare (ignore seq from-end-p))
                                          (some #'< iter limit))
                                        (lambda (seq iter limit from-end-p)
                                          (declare (ignore seq from-end-p))
                                          (some #'>= iter limit)))
                                    (lambda (seq iter)
                                      (row-major-aref (the array seq)
                                                      (apply #'array-row-major-index
                                                             (the array seq) iter)))
                                    (lambda (element seq iter)
                                      (setf (row-major-aref (the array seq)
                                                            (apply #'array-row-major-index
                                                                   (the array seq) iter))
                                            element))
                                    (lambda (seq iter)
                                      (declare (ignore seq))
                                      iter))))
                         (t
                          (let ((indices (make-list (array-rank (the array ,in-ref)))))                                     
                            (values (if ,from-end-ref
                                        (1- (array-total-size (the array ,in-ref)))
                                        0)
                                    (if ,from-end-ref
                                        0
                                        (array-total-size (the array ,in-ref)))
                                    ,from-end-ref
                                    (if ,from-end-ref
                                        (lambda (seq iter from-end-p)
                                          (declare (ignore seq from-end-p))
                                          (1- iter))
                                        (lambda (seq iter from-end-p)
                                          (declare (ignore seq from-end-p))
                                          (1+ iter)))
                                    (if ,from-end-ref
                                        (lambda (seq iter limit from-end-p)
                                          (declare (ignore seq from-end-p))
                                          (< iter limit))
                                        (lambda (seq iter limit from-end-p)
                                          (declare (ignore seq from-end-p))
                                          (>= iter limit)))
                                    #'row-major-aref
                                    (lambda (element seq iter)
                                      (setf (row-major-aref (the array seq) iter) element))
                                    (lambda (array row-major-index)
                                      (mapl (lambda (y x)
                                              (multiple-value-bind (q r)
                                                  (floor row-major-index
                                                         (apply '* (cdr x)))
                                                (setf row-major-index r)
                                                (rplaca y q)))
                                            indices
                                            (array-dimensions array)))))))))
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

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:being-region) (name (eql :element)) &key var)
  (make-instance 'for-as-elements :var var))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:being-region) (name (eql :elements)) &key var)
  (make-instance 'for-as-elements :var var))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:for-as-region) (name (eql :over)) &key var)
  (khazern:unparse-token :in)
  (make-instance 'for-as-elements :var var))
