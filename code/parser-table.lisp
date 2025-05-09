(cl:in-package #:khazern)

(defclass parser-table ()
  ((parsers :accessor parser-table-parsers
            :initarg :parsers
            :initform nil
            :type cl:list)
   (paths :reader parser-table-paths
          :initarg :paths
          :initform (make-hash-table :test #'equal))))

(defmethod parser-enabled-p ((table parser-table) name)
  (and (member name (parser-table-parsers table))
       t))

(defmethod (setf parser-enabled-p) (value (table parser-table) name)
  (if value
      (pushnew name (parser-table-parsers table))
      (setf (parser-table-parsers table) (delete name (parser-table-parsers table))))
  value)

(defmethod copy-parser-table ((table parser-table))
  (make-instance 'parser-table
                 :parsers (copy-list (parser-table-parsers table))
                 :paths (let ((paths (make-hash-table :test #'equal)))
                          (maphash (lambda (k v)
                                     (setf (gethash k paths) v))
                                   (parser-table-paths table))
                          paths)))

(defparameter *parser-table* nil)

(defun loop-path-p (name)
  (and (gethash (symbol-name name) (parser-table-paths *parser-table*)) t))

(defmethod add-path ((table parser-table) constructor &rest names)
  (let ((paths (parser-table-paths table)))
    (mapc (lambda (name)
            (setf (gethash (symbol-name name) paths) constructor))
          names)
    nil))

