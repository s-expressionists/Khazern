(cl:in-package #:khazern-elements)

(defmethod khazern:make-iteration-path
    ((client khazern-extrinsic:extrinsic-client) (name (eql :element))
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-elements)))

(defmethod khazern:make-iteration-path
    ((client khazern-extrinsic:extrinsic-client) (name (eql :elements))
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-elements)))
