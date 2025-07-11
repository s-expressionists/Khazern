(cl:in-package #:khazern-extension)

(defclass cleanup-clause (khazern:clause)
  ((%forms :accessor forms
           :initarg :forms
           :type list)
   (%clause-group :accessor khazern:clause-group
                  :initform :main)))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:body-region) (keyword (eql :cleanup)) &key)
  (make-instance 'cleanup-clause
                 :start khazern:*start*
                 :forms (khazern:parse-compound-forms)
                 :end khazern:*index*))

(defmethod khazern:afterword-forms ((clause cleanup-clause))
  (copy-list (forms clause)))
