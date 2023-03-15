(cl:in-package #:khazern)

(defclass parser-table ()
  ((parsers :accessor parser-table-parsers
            :initarg :parsers
            :initform nil
            :type list)))

(defgeneric copy-parser-table (table))

(defgeneric parser-enabled-p (table name))

(defgeneric (setf parser-enabled-p) (value table name))

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
                 :parsers (copy-seq (parser-table-parsers table))))

(defparameter *parser-table* nil)

