(cl:in-package #:khazern)

(defclass never-clause (termination-test-clause form-mixin) ())

(defmethod accumulation-variables ((clause never-clause))
  `((nil always/never t)))

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
;;; Compute the body-form

(defmethod body-form ((clause never-clause) end-tag)
  (declare (ignore end-tag))
  `(when ,(form clause)
     (return-from ,*loop-name* nil)))
