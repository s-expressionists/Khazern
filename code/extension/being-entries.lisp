(cl:in-package #:khazern-extension)

#+abcl (require :extensible-sequences)

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
                                                             :form '(cons nil nil)
                                                             :ignorable t
                                                             :dynamic-extent t)))
                                                             
(defmethod khazern:preposition-names ((client extension-client) (instance being-entries))
  (values '((:in :of))
          '((:in :of))))

(defun parse-hash-table-in (instance)
  (setf (in-ref instance) (khazern:add-simple-binding instance
                                                      :var "IN"
                                                      :form (khazern:parse-token)
                                                      :type 'hash-table)))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance being-entries) (key (eql :in)))
  (parse-hash-table-in instance))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance being-entries) (key (eql :of)))
  (parse-hash-table-in instance))
  
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
                   (iterator-var iterator-var))
      clause
    `((setf (values ,entry-p-var
                    (car ,key-value-var)
                    (cdr ,key-value-var))
            (,iterator-var))
      (unless ,entry-p-var
        (go ,khazern:*epilogue-tag*)))))

(defmethod khazern:step-outro-forms ((clause being-entries) initialp)
  (declare (ignore initialp))
  (khazern:destructuring-set (var clause)
                             (key-value-var clause)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:being-region) (name (eql :entry)) &key var)
  (make-instance 'being-entries :var var))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:being-region) (name (eql :entries)) &key var)
  (make-instance 'being-entries :var var))
