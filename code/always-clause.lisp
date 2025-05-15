(cl:in-package #:khazern)

(defclass always-clause (boolean-termination-test-clause)
  ())

(defmethod accumulation-category ((clause always-clause))
  'always/never)

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

(defmethod body-forms ((clause always-clause))
  `((unless ,(form clause)
      (return-from ,*loop-name* nil))))
