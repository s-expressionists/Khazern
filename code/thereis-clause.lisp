(cl:in-package #:khazern)

(defclass thereis-clause (boolean-termination-test-clause) ())

(defmethod accumulation-category ((clause thereis-clause))
  'thereis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser thereis-clause (:body-clause)
  (consecutive (lambda (form)
                 (make-instance 'thereis-clause :form form))
               (keyword :thereis)
               'terminal
               'anything))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the body-forms

(defmethod body-forms ((clause thereis-clause))
  `((let ((temp ,(form clause)))
      (when temp
        (return-from ,*loop-name* temp)))))
