(in-package #:khazern-extension)

(defclass extension-client (khazern:standard-client) ())

(defmethod khazern:features-list nconc ((client extension-client))
  (list :loop/khazern-extension
        :loop/iteration-path/bytes 
        :loop/iteration-path/characters 
        :loop/iteration-path/elements 
        :loop/iteration-path/lines 
        :loop/iteration-path/objects 
        :loop/cleanup))
