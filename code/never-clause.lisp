(cl:in-package #:khazern)

(defclass never-clause (termination-test-clause form-mixin) ())

(defmethod map-variables (function (clause never-clause))
  (funcall function (default-accumulation-variable) t 'always/never))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser never-clause (:body-clause)
  (consecutive (lambda (form)
                 (make-instance 'never-clause :form form))
               (keyword :never)
               'terminal
               'anything))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the body-forms

(defmethod body-forms ((clause never-clause) end-tag)
  (declare (ignore end-tag))
  `((when ,(form clause)
     (return-from ,*loop-name* nil))))
