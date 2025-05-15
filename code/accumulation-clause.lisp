(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accumulation clauses

(defclass accumulation-clause (selectable-clause var-mixin)
  ()
  (:default-initargs :var (make-instance 'd-spec
                                         :var-spec (default-accumulation-variable))))

(defmethod initialize-instance :after ((instance accumulation-clause) &rest initargs &key)
  (declare (ignore initargs))
  (unless (var-spec (var instance))
    (setf (var-spec (var instance)) (default-accumulation-variable))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NUMERIC-ACCUMULATION-CLAUSE.

(defclass numeric-accumulation-clause (accumulation-clause) ())

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
           (var-spec (var clause)) (type-spec (var clause))
           (accumulation-category clause)))
