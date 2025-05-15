(cl:in-package #:khazern)

(defclass never-clause (boolean-termination-test-clause)
  ())

(defmethod accumulation-category ((clause never-clause))
  'always/never)

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

(defmethod body-forms ((clause never-clause))
  `((when ,(form clause)
     (return-from ,*loop-name* nil))))
