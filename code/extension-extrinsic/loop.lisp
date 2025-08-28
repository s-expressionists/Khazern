(cl:in-package #:khazern-extension-extrinsic)

(defclass extrinsic-client (khazern-extension:extension-client)
  ())

(defvar *client* (make-instance 'extrinsic-client))

(khazern:define-interface :client-form *client* :client-class extrinsic-client)
