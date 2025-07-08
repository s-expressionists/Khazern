(cl:in-package #:khazern-extension-extrinsic/test)

(defclass binary-input-stream (ngray:fundamental-binary-input-stream)
  ((value :reader value
          :initarg :value)
   (index :accessor index
          :initform 0)))

(defmethod ngray:stream-element-type ((stream binary-input-stream))
  '(unsigned-byte 8))

(defmethod ngray:stream-read-byte ((stream binary-input-stream))
  (with-accessors ((value value)
                   (index index))
      stream
    (if (< index (length value))
        (prog1 (elt value index)
          (incf index))
        :eof)))

(defmacro with-input-from-bytes ((stream bytes) &body body)
  `(let ((,stream (make-instance 'binary-input-stream
                                 :value ,bytes)))
     ,@body))
