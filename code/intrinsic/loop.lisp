(cl:in-package #:khazern-intrinsic)

(defclass intrinsic-client (khazern:standard-client)
  ())

(defvar *client* (make-instance 'intrinsic-client))

(khazern:define-interface :client-form *client* :client-class intrinsic-client :intrinsic t)
