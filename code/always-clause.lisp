(cl:in-package #:khazern)

(defclass always-clause (termination-test-clause form-mixin) ())

(defmethod accumulation-variables ((clause always-clause))
  `((nil always/never t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser always-clause (:body-clause)
  (consecutive (lambda (always form)
                 (declare (ignore always))
                 (make-instance 'always-clause
                   :form form))
               (keyword 'always)
               'anything))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the body-form

(defmethod body-form ((clause always-clause) end-tag)
  (declare (ignore end-tag))
  `(unless ,(form clause)
     (return-from ,*loop-name* nil)))
