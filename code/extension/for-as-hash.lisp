(cl:in-package #:khazern-extension)

(defmethod khazern:parse-iteration-path-using
    ((client extension-client) (instance khazern:for-as-hash) key)
  (declare (ignore key))
  (setf (khazern:other-var instance)
        (khazern:add-binding instance (khazern:parse-d-spec :ignorable t))))

