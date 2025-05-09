(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accumulation clauses

(defclass accumulation-clause (selectable-clause)
  ((%into-var :accessor into-var
              :initarg :into-var
              :initform (default-accumulation-variable))))

(defmethod initialize-instance :after ((instance accumulation-clause) &rest initargs &key)
  (declare (ignore initargs))
  (unless (into-var instance)
    (setf (into-var instance) (default-accumulation-variable))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LIST-ACCUMULATION-CLAUSE.
;;;
;;; This class is the superclass of the list accumulation clauses:
;;; COLLECT-CLAUSE, APPEND-CLAUSE, and NCONC-CLAUSE.
;;;

(defclass list-accumulation-clause (accumulation-clause) ())

(defmethod accumulation-category ((clause list-accumulation-clause))
  'list)

;;; The methods on ACCUMULATION-VARIABLES call the function TYPE-SPEC
;;; on the clause in order to obtain the third element of each
;;; accumulation variable descriptor.  For the numeric accumulation
;;; clauses, the type is stored in a slot.  For the list accumulation
;;; clauses, we always want to return the type LIST.
(defmethod type-spec ((clause list-accumulation-clause))
  'cl:list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NUMERIC-ACCUMULATION-CLAUSE.

(defclass numeric-accumulation-clause (accumulation-clause)
  ((%type-spec :initform T :initarg :type-spec :reader type-spec)))

(defclass count/sum-accumulation-clause (numeric-accumulation-clause) ())

(defmethod accumulation-category ((clause count/sum-accumulation-clause))
  'count/sum)

(defclass max/min-accumulation-clause (numeric-accumulation-clause) ())

(defmethod accumulation-category ((clause max/min-accumulation-clause))
  'max/min)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on ACCUMULATION-VARIABLES, valid for all accumulation
;;; clauses.

(defmethod map-variables (function (clause accumulation-clause))
  (funcall function
           (into-var clause) (type-spec clause)
           (accumulation-category clause)))
