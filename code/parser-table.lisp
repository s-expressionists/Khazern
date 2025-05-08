(cl:in-package #:khazern)

(defclass parser-table ()
  ((parsers :accessor parser-table-parsers
            :initarg :parsers
            :initform nil
            :type cl:list)
   (paths :reader parser-table-paths
          :initarg :paths
          :initform (make-hash-table :test #'equal))))

(defclass path ()
  ((%function :reader path-function
              :initarg :function)
   (%prepositions :reader path-prepositions
                  :initarg :prepositions
                  :initform (make-hash-table :test #'equal))
   (%data :reader path-data
          :initform nil
          :initarg :data)))

(defgeneric add-path (table pathname-or-names path-function list-of-allowable-prepositions data)
  (:method ((table parser-table) pathname-or-names path-function list-of-allowable-prepositions data)
    (let* ((instance (make-path :function path-function
                                :data data))
           (prepositions (path-prepositions instance)))
      (mapc (lambda (names)
              (let ((key (keyword (symbol-name (if (listp names)
                                                   (car names)
                                                   names)))))
                (mapc (lambda (name)
                        (setf (gethash (symbol-name name) prepositions)
                              key))
                      (if (listp names)
                          names
                          (list names)))))
            list-of-allowable-prepositions)
      (mapc (lambda (name)
              (setf (gethash (symbol-name name) (paths table)) instance))
            (if (listp pathname-or-names)
                pathname-or-names
                (list pathname-or-names))))))

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
                 :parsers (copy-list (parser-table-parsers table))
                 :paths (let ((paths (make-hash-table :test #'equal)))
                          (maphash (lambda (k v)
                                     (setf (gethash k paths) v))
                                   (parser-table-paths table))
                          paths)))

(defparameter *parser-table* nil)

(defun loop-path-p (name)
  (and (gethash (symbol-name name) (parser-table-paths *parser-table*)) t))

