(cl:in-package #:khazern-extension)

(defclass for-as-stream ()
  ((%var :accessor var
         :accessor khazern:var
         :initarg :var)
   (%temp-var :reader temp-var
              :initform (make-instance 'khazern:d-spec
                                       :var-spec (gensym "TMP")))
   (%preposition-names :accessor khazern:iteration-path-preposition-names
                       :initarg :preposition-names
                       :initform (list :in :of :close))
   (%using-names :accessor khazern:iteration-path-using-names
                 :initarg :using-names
                 :initform (list :stream))
   (%stream-var :accessor stream-var
                :initform (gensym "STREAM"))
   (%close :accessor closep
           :initform nil)
   (%stream-form :accessor stream-form
                 :initform '*standard-input*)))

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
    (expression (instance for-as-stream) (key (eql :in)))
  (setf (stream-form instance) expression))

(defmethod (setf khazern:iteration-path-preposition)
    (expression (instance for-as-stream) (key (eql :of)))
  (setf (stream-form instance) expression))

(defmethod (setf khazern:iteration-path-preposition)
    (expression (instance for-as-stream) (key (eql :close)))
  (setf (closep instance) expression))

(defmethod (setf khazern:iteration-path-using) :after (expression (instance for-as-stream) key)
  (setf (khazern:iteration-path-using-names instance)
        (delete key (khazern:iteration-path-using-names instance))))

(defmethod (setf khazern:iteration-path-using)
    (value (instance for-as-stream) (key (eql :stream)))
  (setf (stream-var instance) value))

(defmethod khazern:initial-bindings nconc ((clause for-as-stream))
  (nconc `((,(stream-var clause) ,(stream-form clause)))
         (khazern:d-spec-outer-bindings (temp-var clause))
         (khazern:d-spec-outer-bindings (var clause))))

(defmethod khazern:initial-declarations nconc ((clause for-as-stream))
  (nconc `((type stream ,(stream-var clause)))
         (khazern:d-spec-outer-declarations (temp-var clause))
         (khazern:d-spec-outer-declarations (var clause))))

(defmethod khazern:wrap-forms ((clause for-as-stream) forms)
  (if (closep clause)
      `((unwind-protect
             ,(if (cdr forms)
                  `(progn ,@forms)
                  (car forms))
          (close ,(stream-var clause))))
      forms))

(defclass for-as-bytes (for-as-stream) ())

(defmethod khazern:analyze ((instance for-as-bytes))
  (when (eq (khazern::type-spec (var instance)) khazern::*placeholder-result*)
    (setf (khazern::type-spec (var instance)) 'integer))
  (setf (khazern::type-spec (temp-var instance))
        `(or stream ,(khazern::type-spec (var instance)))))

(defun for-as-bytes/step (clause)
  (let ((temp-var (khazern::var-spec (temp-var clause)))
        (stream-var (stream-var clause)))
    `((setq ,temp-var (read-byte ,stream-var nil ,stream-var))
      (when (eq ,temp-var ,stream-var)
        (go ,khazern:*epilogue-tag*))
      ,@(khazern:d-spec-inner-form (var clause) temp-var))))

(defmethod khazern:initial-step-forms ((clause for-as-bytes))
  (for-as-bytes/step clause))

(defmethod khazern:subsequent-step-forms ((clause for-as-bytes))
  (for-as-bytes/step clause))

(defclass for-as-characters (for-as-stream) ())

(defmethod khazern:analyze ((instance for-as-characters))
  (when (eq (khazern::type-spec (var instance)) khazern::*placeholder-result*)
    (setf (khazern::type-spec (var instance)) 'character))
  (setf (khazern::type-spec (temp-var instance))
        `(or stream ,(khazern::type-spec (var instance)))))

(defun for-as-characters/step (clause)
  (let ((temp-var (khazern::var-spec (temp-var clause)))
        (stream-var (stream-var clause)))
    `((setq ,temp-var (read-char ,stream-var nil ,stream-var))
      (when (eq ,temp-var ,stream-var)
        (go ,khazern:*epilogue-tag*))
      ,@(khazern:d-spec-inner-form (var clause) temp-var))))

(defmethod khazern:initial-step-forms ((clause for-as-characters))
  (for-as-characters/step clause))

(defmethod khazern:subsequent-step-forms ((clause for-as-characters))
  (for-as-characters/step clause))

(defclass for-as-objects (for-as-stream) ())

(defmethod khazern:analyze ((instance for-as-objects))
  (when (eq (khazern::type-spec (var instance)) khazern::*placeholder-result*)
    (setf (khazern::type-spec (var instance)) t))
  (unless (eq (khazern::type-spec (var instance)) t)
    (setf (khazern::type-spec (temp-var instance))
          `(or stream ,(khazern::type-spec (var instance))))))

(defun for-as-objects/step (clause)
  (let ((temp-var (khazern::var-spec (temp-var clause)))
        (stream-var (stream-var clause)))
    `((setq ,temp-var (read ,stream-var nil ,stream-var))
      (when (eq ,temp-var ,stream-var)
        (go ,khazern:*epilogue-tag*))
      ,@(khazern:d-spec-inner-form (var clause) temp-var))))

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
  (when (eq (khazern::type-spec (var instance)) khazern::*placeholder-result*)
    (setf (khazern::type-spec (var instance)) 'string))
  (unless (eq (khazern::type-spec (var instance)) t)
    (setf (khazern::type-spec (temp-var instance))
          `(or stream ,(khazern::type-spec (var instance))))))

(defmethod khazern:initial-bindings nconc ((clause for-as-lines))
  `((,(missing-newline-p-var clause) nil)))

(defmethod khazern:initial-declarations nconc ((clause for-as-lines))
  `((ignorable ,(missing-newline-p-var clause))))

(defun for-as-lines/step (clause)
  (let ((temp-var (khazern::var-spec (temp-var clause)))
        (missing-newline-p-var (missing-newline-p-var clause))
        (stream-var (stream-var clause)))
    `((multiple-value-setq (,temp-var ,missing-newline-p-var)
        (read-line ,stream-var nil ,stream-var))
      (when (eq ,temp-var ,stream-var)
        (go ,khazern:*epilogue-tag*))
      ,@(khazern:d-spec-inner-form (var clause) temp-var))))

(defmethod khazern:initial-step-forms ((clause for-as-lines))
  (for-as-lines/step clause))

(defmethod khazern:subsequent-step-forms ((clause for-as-lines))
  (for-as-lines/step clause))
