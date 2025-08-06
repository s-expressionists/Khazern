(cl:in-package #:khazern-extension)

(defclass being-stream-values (khazern:clause)
  ((%var :accessor var
         :initarg :var)
   (%temp-ref :accessor temp-ref)
   (%temp-var :accessor temp-var)
   (%stream-ref :accessor stream-ref
                :initform nil)
   (%stream-d-spec :accessor stream-d-spec
                   :initform nil)
   (%close-ref :accessor close-ref
               :initform nil)
   (%stream-var :accessor stream-var
                :initform nil)))

(defmethod initialize-instance :after ((instance being-stream-values) &rest initargs &key)
  (declare (ignore initargs))
  (khazern:add-binding instance (var instance)))

(defmethod khazern:preposition-names ((client extension-client) (instance being-stream-values))
  (values '((:in :of) :close)
          '()
          '(:stream)))

(defun parse-stream-of (instance)
  (setf (values (stream-ref instance) (stream-d-spec instance))
        (khazern:add-simple-binding instance :var (or (stream-var instance) "STREAM")
                                             :form (khazern:parse-token) :type 'stream)))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance being-stream-values) (key (eql :in)))
  (parse-stream-of instance))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance being-stream-values) (key (eql :of)))
  (parse-stream-of instance))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance being-stream-values) (key (eql :close)))
  (setf (close-ref instance) (khazern:add-simple-binding instance
                                                         :var "CLOSEP"
                                                         :form (khazern:parse-token))))

(defmethod khazern:parse-using
    ((client extension-client) (instance being-stream-values) (key (eql :stream)))
  (let ((value (khazern:parse-token :type 'khazern:simple-var)))
    (when (stream-d-spec instance)
      (setf (khazern:var-spec (stream-d-spec instance)) value
            (stream-ref instance) value))
    (setf (stream-var instance) value)))

(defmethod khazern:analyze :before ((client extension-client) (instance being-stream-values))
  (cond ((stream-ref instance))
        ((stream-var instance)
         (setf (stream-ref instance) (khazern:add-simple-binding instance
                                                                 :var (stream-var instance)
                                                                 :form '*standard-input*
                                                                 :type 'stream)))
        (t
         (setf (stream-ref instance) '*standard-input*))))

(defmethod khazern:analyze :after ((client extension-client) (instance being-stream-values))
  (khazern:check-nullable-simple-var-spec (var instance))
  (setf (values (temp-ref instance) (temp-var instance))
        (khazern:add-simple-binding instance
                                    :var "NEXT"
                                    :type `(or stream
                                               ,(khazern:type-spec (var instance))))))

(defmethod khazern:afterword-forms ((clause being-stream-values))
  (when (close-ref clause)
    `((when ,(close-ref clause)
        (close ,(stream-ref clause))))))

(defmethod khazern:step-intro-forms ((clause being-stream-values) initialp)
  (declare (ignore initialp))
  `((when (eq ,(temp-ref clause) ,(stream-ref clause))
      (go ,khazern:*epilogue-tag*))))

(defmethod khazern:step-outro-forms ((clause being-stream-values) initialp)
  (declare (ignore initialp))
  (khazern:expand-assignments (var clause) (temp-ref clause)))

(defclass being-bytes (being-stream-values) ())

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:being-region) (name (eql :byte)) &key var)
  (make-instance 'being-bytes :var var :start khazern:*start*))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:being-region) (name (eql :bytes)) &key var)
  (make-instance 'being-bytes :var var :start khazern:*start*))

(defmethod khazern:analyze ((client extension-client) (instance being-bytes))
  (when (eq (khazern:type-spec (var instance)) khazern:*placeholder-result*)
    (setf (khazern:type-spec (var instance)) 'integer)))

(defmethod khazern:step-intro-forms :around ((clause being-bytes) initialp)
  (declare (ignore initialp))
  (let ((temp-var (temp-ref clause))
        (stream-ref (stream-ref clause)))
    (list* `(setq ,temp-var (read-byte ,stream-ref nil ,stream-ref))
           (call-next-method))))

(defclass being-characters (being-stream-values) ())

(defmethod khazern:analyze ((client extension-client) (instance being-characters))
  (when (eq (khazern:type-spec (var instance)) khazern:*placeholder-result*)
    (setf (khazern:type-spec (var instance)) 'character)))

(defmethod khazern:step-intro-forms :around ((clause being-characters) initialp)
  (declare (ignore initialp))
  (let ((temp-var (temp-ref clause))
        (stream-ref (stream-ref clause)))
    (list* `(setq ,temp-var (read-char ,stream-ref nil ,stream-ref))
           (call-next-method))))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:being-region) (name (eql :character)) &key var)
  (make-instance 'being-characters :var var :start khazern:*start*))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:being-region) (name (eql :characters)) &key var)
  (make-instance 'being-characters :var var :start khazern:*start*))

(defclass being-objects (being-stream-values) ())

(defmethod khazern:analyze ((client extension-client) (instance being-objects))
  (when (eq (khazern:type-spec (var instance)) khazern:*placeholder-result*)
    (setf (khazern:type-spec (var instance)) t)))

(defmethod khazern:step-intro-forms :around ((clause being-objects) initialp)
  (declare (ignore initialp))
  (let ((temp-var (temp-ref clause))
        (stream-ref (stream-ref clause)))
    (list* `(setq ,temp-var (read ,stream-ref nil ,stream-ref))
           (call-next-method))))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:being-region) (name (eql :object)) &key var)
  (make-instance 'being-objects :var var :start khazern:*start*))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:being-region) (name (eql :objects)) &key var)
  (make-instance 'being-objects :var var :start khazern:*start*))

(defclass being-lines (being-stream-values)
  ((%missing-newline-p-var :accessor missing-newline-p-var
                           :initform (gensym "MISS"))))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:being-region) (name (eql :line)) &key var)
  (make-instance 'being-lines :var var :start khazern:*start*))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:being-region) (name (eql :lines)) &key var)
  (make-instance 'being-lines :var var :start khazern:*start*))

(defmethod khazern:preposition-names ((client extension-client) (instance being-stream-values))
  (values '((:in :of) :close)
          '()
          '(:stream :missing-newline-p)))

(defmethod khazern:parse-using
    ((client extension-client) (instance being-lines) (key (eql :missing-newline-p)))
  (setf (missing-newline-p-var instance)
        (khazern:add-simple-binding instance
                                    :var (khazern:parse-token :type 'khazern:simple-var)
                                    :ignorable t)))

(defmethod khazern:analyze ((client extension-client) (instance being-lines))
  (when (eq (khazern:type-spec (var instance)) khazern:*placeholder-result*)
    (setf (khazern:type-spec (var instance)) 'string)))

(defmethod khazern:step-intro-forms :around ((clause being-lines) initialp)
  (declare (ignore initialp))
  (let ((temp-var (temp-ref clause))
        (missing-newline-p-var (missing-newline-p-var clause))
        (stream-ref (stream-ref clause)))
    (list* `(multiple-value-setq (,temp-var ,missing-newline-p-var)
              (read-line ,stream-ref nil ,stream-ref))
           (call-next-method))))
