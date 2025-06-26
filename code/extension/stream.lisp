(cl:in-package #:khazern-extension)

(defclass for-as-stream (khazern:for-as-iteration-path)
  ((%temp-var :reader temp-var
              :initform (make-instance 'khazern:d-spec
                                       :var-spec (gensym "TMP")))
   (%stream-ref :accessor stream-ref
                :initform nil)
   (%stream-d-spec :accessor stream-d-spec
                   :initform nil)
   (%close-ref :accessor close-ref
               :initform nil)
   (%stream-var :accessor stream-var
                :initform nil))
  (:default-initargs :preposition-names (list :in :of :close)
                     :using-names (list :stream)))

(defmethod khazern:map-variables (function (clause for-as-stream))
  (khazern:map-variables function (khazern:var clause)))

(defmethod (setf khazern:iteration-path-preposition) :after
    (expression (instance for-as-stream) key)
  (setf (khazern:iteration-path-preposition-names instance)
        (delete-if (lambda (name)
                     (or (eq name key)
                         (and (eq key :in)
                              (eq name :of))
                         (and (eq key :of)
                              (eq name :in))))
                   (khazern:iteration-path-preposition-names instance))))

(defmethod (setf khazern:iteration-path-preposition)
    (value (instance for-as-stream) (key (eql :in)))
  (setf (values (stream-ref instance) (stream-d-spec instance))
        (khazern:add-simple-binding instance :var (or (stream-var instance) "STREAM")
                                             :form value :type 'stream))
  value)

(defmethod (setf khazern:iteration-path-preposition)
    (value (instance for-as-stream) (key (eql :of)))
  (setf (values (stream-ref instance) (stream-d-spec instance))
        (khazern:add-simple-binding instance :var (or (stream-var instance) "STREAM")
                                             :form value :type 'stream))
  value)

(defmethod (setf khazern:iteration-path-preposition)
    (value (instance for-as-stream) (key (eql :close)))
  (setf (close-ref instance) (khazern:add-simple-binding instance :var "CLOSEP" :form value))
  value)

(defmethod (setf khazern:iteration-path-using) :after (expression (instance for-as-stream) key)
  (setf (khazern:iteration-path-using-names instance)
        (delete key (khazern:iteration-path-using-names instance))))

(defmethod (setf khazern:iteration-path-using)
    (value (instance for-as-stream) (key (eql :stream)))
  (when (stream-d-spec instance)
    (setf (khazern::var-spec (stream-d-spec instance)) value
          (stream-ref instance) value))
  (setf (stream-var instance) value))

(defmethod khazern:analyze :before ((instance for-as-stream))
  (cond ((stream-ref instance))
        ((stream-var instance)
         (setf (stream-ref instance) (khazern:add-simple-binding instance
                                                                 :var (stream-var instance)
                                                                 :form '*standard-input*
                                                                 :type 'stream)))
        (t
         (setf (stream-ref instance) '*standard-input*))))

(defmethod khazern:initial-bindings nconc ((clause for-as-stream))
  (nconc (khazern:d-spec-outer-bindings (temp-var clause))
         (khazern:d-spec-outer-bindings (khazern:var clause))))

(defmethod khazern:initial-declarations nconc ((clause for-as-stream))
  (nconc (khazern:d-spec-outer-declarations (temp-var clause))
         (khazern:d-spec-outer-declarations (khazern:var clause))))

(defmethod khazern:wrap-forms ((clause for-as-stream) forms)
  (if (close-ref clause)
      `((unwind-protect
             ,(if (cdr forms)
                  `(progn ,@forms)
                  (car forms))
          (when ,(close-ref clause)
            (close ,(stream-ref clause)))))
      forms))

(defclass for-as-bytes (for-as-stream) ())

(defmethod khazern:analyze ((instance for-as-bytes))
  (when (eq (khazern::type-spec (khazern:var instance)) khazern::*placeholder-result*)
    (setf (khazern::type-spec (khazern:var instance)) 'integer))
  (setf (khazern::type-spec (temp-var instance))
        `(or stream ,(khazern::type-spec (khazern:var instance)))))

(defun for-as-bytes/step (clause)
  (let ((temp-var (khazern::var-spec (temp-var clause)))
        (stream-ref (stream-ref clause)))
    `((setq ,temp-var (read-byte ,stream-ref nil ,stream-ref))
      (when (eq ,temp-var ,stream-ref)
        (go ,khazern:*epilogue-tag*))
      ,@(khazern:d-spec-inner-form (khazern:var clause) temp-var))))

(defmethod khazern:initial-step-forms ((clause for-as-bytes))
  (for-as-bytes/step clause))

(defmethod khazern:subsequent-step-forms ((clause for-as-bytes))
  (for-as-bytes/step clause))

(defclass for-as-characters (for-as-stream) ())

(defmethod khazern:analyze ((instance for-as-characters))
  (when (eq (khazern::type-spec (khazern:var instance)) khazern::*placeholder-result*)
    (setf (khazern::type-spec (khazern:var instance)) 'character))
  (setf (khazern::type-spec (temp-var instance))
        `(or stream ,(khazern::type-spec (khazern:var instance)))))

(defun for-as-characters/step (clause)
  (let ((temp-var (khazern::var-spec (temp-var clause)))
        (stream-ref (stream-ref clause)))
    `((setq ,temp-var (read-char ,stream-ref nil ,stream-ref))
      (when (eq ,temp-var ,stream-ref)
        (go ,khazern:*epilogue-tag*))
      ,@(khazern:d-spec-inner-form (khazern:var clause) temp-var))))

(defmethod khazern:initial-step-forms ((clause for-as-characters))
  (for-as-characters/step clause))

(defmethod khazern:subsequent-step-forms ((clause for-as-characters))
  (for-as-characters/step clause))

(defclass for-as-objects (for-as-stream) ())

(defmethod khazern:analyze ((instance for-as-objects))
  (when (eq (khazern::type-spec (khazern:var instance)) khazern::*placeholder-result*)
    (setf (khazern::type-spec (khazern:var instance)) t))
  (unless (eq (khazern::type-spec (khazern:var instance)) t)
    (setf (khazern::type-spec (temp-var instance))
          `(or stream ,(khazern::type-spec (khazern:var instance))))))

(defun for-as-objects/step (clause)
  (let ((temp-var (khazern::var-spec (temp-var clause)))
        (stream-ref (stream-ref clause)))
    `((setq ,temp-var (read ,stream-ref nil ,stream-ref))
      (when (eq ,temp-var ,stream-ref)
        (go ,khazern:*epilogue-tag*))
      ,@(khazern:d-spec-inner-form (khazern:var clause) temp-var))))

(defmethod khazern:initial-step-forms ((clause for-as-objects))
  (for-as-objects/step clause))

(defmethod khazern:subsequent-step-forms ((clause for-as-objects))
  (for-as-objects/step clause))

(defclass for-as-lines (for-as-stream)
  ((%missing-newline-p-var :accessor missing-newline-p-var
                           :initform (gensym "MISS")))
  (:default-initargs :using-names (list :stream :missing-newline-p)))

(defmethod (setf khazern:iteration-path-using)
    (value (instance for-as-lines) (key (eql :missing-newline-p)))
  (setf (missing-newline-p-var instance) value))

(defmethod khazern:analyze ((instance for-as-lines))
  (when (eq (khazern::type-spec (khazern:var instance)) khazern::*placeholder-result*)
    (setf (khazern::type-spec (khazern:var instance)) 'string))
  (unless (eq (khazern::type-spec (khazern:var instance)) t)
    (setf (khazern::type-spec (temp-var instance))
          `(or stream ,(khazern::type-spec (khazern:var instance))))))

(defmethod khazern:initial-bindings nconc ((clause for-as-lines))
  `((,(missing-newline-p-var clause) nil)))

(defmethod khazern:initial-declarations nconc ((clause for-as-lines))
  `((ignorable ,(missing-newline-p-var clause))))

(defun for-as-lines/step (clause)
  (let ((temp-var (khazern::var-spec (temp-var clause)))
        (missing-newline-p-var (missing-newline-p-var clause))
        (stream-ref (stream-ref clause)))
    `((multiple-value-setq (,temp-var ,missing-newline-p-var)
        (read-line ,stream-ref nil ,stream-ref))
      (when (eq ,temp-var ,stream-ref)
        (go ,khazern:*epilogue-tag*))
      ,@(khazern:d-spec-inner-form (khazern:var clause) temp-var))))

(defmethod khazern:initial-step-forms ((clause for-as-lines))
  (for-as-lines/step clause))

(defmethod khazern:subsequent-step-forms ((clause for-as-lines))
  (for-as-lines/step clause))
