(cl:in-package #:khazern)

(defclass while-clause (termination-test-clause form-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser while-clause (:body-clause)
  (consecutive (lambda (form)
                 (make-instance 'while-clause :form form))
               (keyword :while)
               'terminal
               'anything))

(define-parser until-clause (:body-clause)
  (consecutive (lambda (form)
                 (make-instance 'while-clause :form `(not ,form)))
               (keyword :until)
               'terminal
               'anything))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the body-forms

(defmethod body-forms ((clause while-clause))
  `((unless ,(form clause)
      (go ,*epilogue-tag*))))
