(cl:in-package #:khazern-extension)

(defclass being-entries (khazern:clause)
  ((%var :accessor var
         :initarg :var)
   (%entry-p-var :accessor entry-p-var)
   (%key-value-var :accessor key-value-var)
   (%iterator-var :reader iterator-var
                  :initform (khazern:unique-name :iter))
   (%in-ref :accessor in-ref
            :initform nil)))

(defmethod initialize-instance :after ((instance being-entries) &rest initargs &key)
  (declare (ignore initargs))
  (khazern:add-binding instance (var instance))
  (setf (entry-p-var instance) (khazern:add-simple-binding instance :var :entryp)
        #+(or)(key-value-var instance) #+(or)(khazern:add-simple-binding instance
                                                             :var (khazern::temp-variables (var instance))
                                                             :type (khazern::temp-types (var instance))
                                                             :ignorable t)))
                                                             
(defmethod khazern:preposition-names ((client extension-client) (instance being-entries))
  (values '((:in :of))
          '((:in :of))
          '()))

(defun parse-being-entries-of (instance)
  (setf (in-ref instance) (khazern:add-simple-binding instance
                                                      :var :in
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
    (setf (key-value-var instance) (khazern:add-simple-binding instance
                                                             :var (khazern::temp-variables (var instance))
                                                             :type (khazern::temp-types (var instance))
                                                             :ignorable t))

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
    `((setf (values ,entry-p-var ,@(if (consp key-value-var)
                                       key-value-var
                                       (list key-value-var)))
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
