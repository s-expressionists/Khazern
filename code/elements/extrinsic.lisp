(cl:in-package #:khazern-elements)

(defmethod khazern:make-path-iterator
    ((client khazern-extrinsic:extrinsic-client) scope (name (eql :element))
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore scope inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-elements)))

(defmethod khazern:make-path-iterator
    ((client khazern-extrinsic:extrinsic-client) scope (name (eql :elements))
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore scope inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-elements)))
