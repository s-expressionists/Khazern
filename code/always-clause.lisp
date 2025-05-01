(cl:in-package #:khazern)

(defclass always-clause (termination-test-clause form-mixin) ())

(defmethod map-variables (function (clause always-clause))
  (funcall function (default-accumulation-variable) t 'always/never))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser always-clause (:body-clause)
  (consecutive (lambda (form)
                 (make-instance 'always-clause
                   :form form))
               (keyword :always)
               'terminal
               'anything))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the body-forms

(defmethod body-forms ((clause always-clause) end-tag)
  (declare (ignore end-tag))
  `((unless ,(form clause)
      (return-from ,*loop-name* nil))))
