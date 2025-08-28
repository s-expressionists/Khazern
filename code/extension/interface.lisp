(in-package #:khazern-extension)

(defclass extension-client (khazern:standard-client) ())

(defmethod trinsic:features-list nconc ((client extension-client))
  (list :loop/khazern-extension))
