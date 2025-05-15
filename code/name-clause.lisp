(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause NAME-CLAUSE.
;;;
;;; A NAME-CLAUSE is a clause that gives a name to the loop.  It
;;; translates to a block name, so that RETURN-FROM can be used to
;;; exit the loop.  By default, the name of the loop is nil.
;;;
;;; The name-clause is optional, and if present, must be the first one
;;; in the body.  The syntax is:
;;;
;;;    NAMED name
;;;
;;; where name is a symbol.

(defclass name-clause (clause)
  ((%name :reader name
          :initarg :name)))

(defmethod name-clause-p ((clause name-clause))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser.

(define-parser name-clause (:body-clause)
  (consecutive (lambda (name)
                 (make-instance 'name-clause :name name))
               (keyword :named)
               'terminal 
               (typep 'symbol)))
