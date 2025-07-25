(cl:in-package #:khazern-extension)

(defclass for-as-stream (khazern:clause)
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

(defmethod initialize-instance :after ((instance for-as-stream) &rest initargs &key)
  (declare (ignore initargs))
  (khazern:add-binding instance (var instance)))

(defmethod khazern:preposition-names ((client extension-client) (instance for-as-stream))
  (values '((:in :of) :close)
          '()
          '(:stream)))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance for-as-stream) (key (eql :in)))
  (setf (values (stream-ref instance) (stream-d-spec instance))
        (khazern:add-simple-binding instance :var (or (stream-var instance) "STREAM")
                                             :form (khazern:parse-token) :type 'stream)))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance for-as-stream) (key (eql :of)))
  (setf (values (stream-ref instance) (stream-d-spec instance))
        (khazern:add-simple-binding instance :var (or (stream-var instance) "STREAM")
                                             :form (khazern:parse-token) :type 'stream)))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance for-as-stream) (key (eql :close)))
  (setf (close-ref instance) (khazern:add-simple-binding instance
                                                         :var "CLOSEP"
                                                         :form (khazern:parse-token))))

(defmethod khazern:parse-using
    ((client extension-client) (instance for-as-stream) (key (eql :stream)))
  (let ((value (khazern:parse-token :type 'khazern:simple-var)))
    (when (stream-d-spec instance)
      (setf (khazern:var-spec (stream-d-spec instance)) value
            (stream-ref instance) value))
    (setf (stream-var instance) value)))

(defmethod khazern:analyze :before ((client extension-client) (instance for-as-stream))
  (cond ((stream-ref instance))
        ((stream-var instance)
         (setf (stream-ref instance) (khazern:add-simple-binding instance
                                                                 :var (stream-var instance)
                                                                 :form '*standard-input*
                                                                 :type 'stream)))
        (t
         (setf (stream-ref instance) '*standard-input*))))

(defmethod khazern:analyze :after ((client extension-client) (instance for-as-stream))
  (khazern:check-nullable-simple-var-spec (var instance))
  (setf (values (temp-ref instance) (temp-var instance))
        (khazern:add-simple-binding instance
                                    :var "NEXT"
                                    :type `(or stream
                                               ,(khazern:type-spec (var instance))))))

(defmethod khazern:afterword-forms ((clause for-as-stream))
  (when (close-ref clause)
    `((when ,(close-ref clause)
        (close ,(stream-ref clause))))))

(defmethod khazern:step-intro-forms ((clause for-as-stream) initialp)
  (declare (ignore initialp))
  `((when (eq ,(temp-ref clause) ,(stream-ref clause))
      (go ,khazern:*epilogue-tag*))))

(defmethod khazern:step-outro-forms ((clause for-as-stream) initialp)
  (declare (ignore initialp))
  (khazern:destructuring-set (var clause)
                             (temp-ref clause)))

(defclass for-as-bytes (for-as-stream) ())

(defmethod khazern:make-iteration-path
    ((client extension-client) (name (eql :byte)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-bytes :var var)))

(defmethod khazern:make-iteration-path
    ((client extension-client) (name (eql :bytes)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-bytes :var var)))

(defmethod khazern:analyze ((client extension-client) (instance for-as-bytes))
  (when (eq (khazern:type-spec (var instance)) khazern:*placeholder-result*)
    (setf (khazern:type-spec (var instance)) 'integer)))

(defmethod khazern:step-intro-forms :around ((clause for-as-bytes) initialp)
  (declare (ignore initialp))
  (let ((temp-var (temp-ref clause))
        (stream-ref (stream-ref clause)))
    (list* `(setq ,temp-var (read-byte ,stream-ref nil ,stream-ref))
           (call-next-method))))

(defclass for-as-characters (for-as-stream) ())

(defmethod khazern:analyze ((client extension-client) (instance for-as-characters))
  (when (eq (khazern:type-spec (var instance)) khazern:*placeholder-result*)
    (setf (khazern:type-spec (var instance)) 'character)))

(defmethod khazern:step-intro-forms :around ((clause for-as-characters) initialp)
  (declare (ignore initialp))
  (let ((temp-var (temp-ref clause))
        (stream-ref (stream-ref clause)))
    (list* `(setq ,temp-var (read-char ,stream-ref nil ,stream-ref))
           (call-next-method))))

(defmethod khazern:make-iteration-path
    ((client extension-client) (name (eql :character)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-characters :var var)))

(defmethod khazern:make-iteration-path
    ((client extension-client) (name (eql :characters)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-characters :var var)))

(defclass for-as-objects (for-as-stream) ())

(defmethod khazern:analyze ((client extension-client) (instance for-as-objects))
  (when (eq (khazern:type-spec (var instance)) khazern:*placeholder-result*)
    (setf (khazern:type-spec (var instance)) t)))

(defmethod khazern:step-intro-forms :around ((clause for-as-objects) initialp)
  (declare (ignore initialp))
  (let ((temp-var (temp-ref clause))
        (stream-ref (stream-ref clause)))
    (list* `(setq ,temp-var (read ,stream-ref nil ,stream-ref))
           (call-next-method))))

(defmethod khazern:make-iteration-path
    ((client extension-client) (name (eql :object)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-objects :var var)))

(defmethod khazern:make-iteration-path
    ((client extension-client) (name (eql :objects)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-objects :var var)))

(defclass for-as-lines (for-as-stream)
  ((%missing-newline-p-var :accessor missing-newline-p-var
                           :initform (gensym "MISS"))))

(defmethod khazern:make-iteration-path
    ((client extension-client) (name (eql :line)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-lines :var var)))

(defmethod khazern:make-iteration-path
    ((client extension-client) (name (eql :lines)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-lines :var var)))

(defmethod khazern:preposition-names ((client extension-client) (instance for-as-stream))
  (values '((:in :of) :close)
          '()
          '(:stream :missing-newline-p)))

(defmethod khazern:parse-using
    ((client extension-client) (instance for-as-lines) (key (eql :missing-newline-p)))
  (setf (missing-newline-p-var instance)
        (khazern:add-simple-binding instance
                                    :var (khazern:parse-token :type 'khazern:simple-var)
                                    :ignorable t)))

(defmethod khazern:analyze ((client extension-client) (instance for-as-lines))
  (when (eq (khazern:type-spec (var instance)) khazern:*placeholder-result*)
    (setf (khazern:type-spec (var instance)) 'string)))

(defmethod khazern:step-intro-forms :around ((clause for-as-lines) initialp)
  (declare (ignore initialp))
  (let ((temp-var (temp-ref clause))
        (missing-newline-p-var (missing-newline-p-var clause))
        (stream-ref (stream-ref clause)))
    (list* `(multiple-value-setq (,temp-var ,missing-newline-p-var)
              (read-line ,stream-ref nil ,stream-ref))
           (call-next-method))))
