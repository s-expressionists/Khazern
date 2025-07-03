(cl:in-package #:khazern-intrinsic)

(defclass intrinsic-client (khazern:standard-client)
  ())

(defvar *client* (make-instance 'intrinsic-client))

(trivial-package-locks:with-unlocked-system-packages
  (khazern:define-interface *client* intrinsic-client t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :loop/khazern *features*))
