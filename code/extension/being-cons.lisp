(cl:in-package #:khazern-extension)

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:being-region) (name (eql :car)) &key var)
  (make-instance 'khazern:being-cars :var var :start khazern:*start*))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:being-region) (name (eql :cars)) &key var)
  (make-instance 'khazern:being-cars :var var :start khazern:*start*))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:being-region) (name (eql :list)) &key var)
  (make-instance 'khazern:being-lists :var var :start khazern:*start*))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:being-region) (name (eql :lists)) &key var)
  (make-instance 'khazern:being-lists :var var :start khazern:*start*))
