(in-package #:khazern-extrinsic)

(defclass extrinsic-client (khazern:standard-client)
  ())

(defvar *client* (make-instance 'extrinsic-client))

(khazern:define-interface *client* extrinsic-client)
