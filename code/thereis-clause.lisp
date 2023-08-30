(cl:in-package #:khazern)

(defclass thereis-clause (termination-test-clause form-mixin) ())

(defmethod accumulation-variables ((clause thereis-clause))
  `((nil thereis t)))

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

(defmethod body-forms ((clause thereis-clause) end-tag)
  (declare (ignore end-tag))
  `((let ((temp ,(form clause)))
      (when temp
        (return-from ,*loop-name* temp)))))
