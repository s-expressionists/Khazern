(cl:in-package #:khazern-extension)

(defclass being-entries (khazern:clause)
  ((%var :accessor var
         :initarg :var)
   (%entry-p-var :accessor entry-p-var)
   (%key-value-var :accessor key-value-var)
   (%iterator-var :reader iterator-var
                  :initform (gensym "ITER"))
   (%in-ref :accessor in-ref
            :initform nil)))

(defmethod initialize-instance :after ((instance being-entries) &rest initargs &key)
  (declare (ignore initargs))
  (khazern:add-binding instance (var instance))
  (setf (entry-p-var instance) (khazern:add-simple-binding instance :var "ENTRYP")
        (key-value-var instance) (khazern:add-simple-binding instance
                                                             :var "KV"
                                                             :form '(list nil nil)
                                                             :ignorable t)))
                                                             
(defmethod khazern:preposition-names ((client extension-client) (instance being-entries))
  (values '((:in :of))
          '((:in :of))
          '()))

(defun parse-being-entries-of (instance)
  (setf (in-ref instance) (khazern:add-simple-binding instance
                                                      :var "IN"
                                                      :form (khazern:parse-token)
                                                      :type 'hash-table)))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance being-entries) (key (eql :in)))
  (parse-being-entries-of instance))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance being-entries) (key (eql :of)))
  (parse-being-entries-of instance))
  
(defmethod khazern:analyze ((client extension-client) (instance being-entries))
  (when (eq (khazern:type-spec (var instance)) khazern:*placeholder-result*)
    (setf (khazern:type-spec (var instance)) t))
  #+(or)(khazern:check-type-spec (var instance)))

(defmethod khazern:wrap-forms ((subclause being-entries) forms)
  `((with-hash-table-iterator
        (,(iterator-var subclause) ,(in-ref subclause))
      ,@forms)))

(defmethod khazern:step-intro-forms ((clause being-entries) initialp)
  (declare (ignore initialp))
  (with-accessors ((entry-p-var entry-p-var)
                   (key-value-var key-value-var)
                   (iterator-var iterator-var)
                   (var var))
      clause
    `((setf ,@(unless (consp (khazern:var-spec var))
                `(,key-value-var (list nil nil)))
            (values ,entry-p-var
                    (first ,key-value-var)
                    (second ,key-value-var))
            (,iterator-var))
      (unless ,entry-p-var
        (go ,khazern:*epilogue-tag*)))))

(defmethod khazern:step-outro-forms ((clause being-entries) initialp)
  (declare (ignore initialp))
  (khazern:expand-assignments (var clause) (key-value-var clause)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:being-region) (name (eql :entry)) &key var)
  (make-instance 'being-entries :var var :start khazern:*start*))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:being-region) (name (eql :entries)) &key var)
  (make-instance 'being-entries :var var :start khazern:*start*))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:for-as-region) (name (eql :of)) &key var)
  (khazern:unparse-token :of)
  (make-instance 'being-entries :var var :start khazern:*start*))
