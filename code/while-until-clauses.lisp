(cl:in-package #:khazern)

(defclass while-clause (termination-test-clause)
  ((%form :initarg :form :reader form)))

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
;;; Compute the body-form

(defmethod body-form ((clause while-clause) end-tag)
  `(unless ,(form clause)
     (go ,end-tag)))
