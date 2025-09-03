(cl:in-package #:khazern-extension-intrinsic)

(defclass intrinsic-client (khazern-extension:extension-client)
  ())

(defvar *client* (make-instance 'intrinsic-client))

(trivial-package-locks:with-unlocked-system-packages
  (khazern:define-interface :client-form *client* :client-class intrinsic-client :intrinsic t))
