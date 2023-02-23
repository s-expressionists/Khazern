(cl:in-package #:khazern)

(defclass while-clause (termination-test-clause)
  ((%form :initarg :form :reader form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser while-clause (:body-clause)
  (consecutive (lambda (while form)
                 (declare (ignore while))
                 (make-instance 'while-clause
                   :form form))
               (keyword 'while)
               'anything))

(define-parser until-clause (:body-clause)
  (consecutive (lambda (until form)
                 (declare (ignore until))
                 (make-instance 'while-clause
                   :form `(not ,form)))
               (keyword 'until)
               'anything))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the body-form

(defmethod body-form ((clause while-clause) end-tag)
  `(unless ,(form clause)
     (go ,end-tag)))
