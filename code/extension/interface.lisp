(in-package #:khazern-extension)

(defclass extension-client (khazern:standard-client) ())

(defmethod khazern:features-list nconc ((client extension-client))
  (list :loop/khazern-extension
        :loop/being/bytes 
        :loop/being/characters 
        :loop/being/elements 
        :loop/being/lines 
        :loop/being/objects 
        :loop/cleanup))
