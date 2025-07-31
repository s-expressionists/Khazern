(cl:in-package #:khazern-extension)

(defun parse-being-hash-entries-other (instance)
  (setf (khazern:other-var instance)
        (khazern:add-binding instance (khazern:parse-d-spec :ignorable t))))

(defmethod khazern:parse-using
    ((client extension-client) (instance khazern:being-hash-keys) (key (eql :hash-value)))
  (parse-being-hash-entries-other instance))

(defmethod khazern:parse-using
    ((client extension-client) (instance khazern:being-hash-values) (key (eql :hash-key)))
  (parse-being-hash-entries-other instance))
