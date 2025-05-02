(cl:in-package #:khazern)

(defclass minimize-clause (max/min-accumulation-clause form-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser minimize-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var type-spec)
                 (make-instance 'minimize-clause
                                :form form
                                :into-var var
                                :type-spec (type-or-null type-spec)))
               (keyword :minimize :minimizing)
               'terminal
               'anything
               'optional-into-phrase
               'optional-type-spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.

(defmethod body-forms ((clause minimize-clause) end-tag)
  (declare (ignore end-tag))
  (let ((form (form clause)))
    (when (and *it-var* (it-keyword-p form))
      (setf form *it-var*))
    `((cond ((null ,(into-var clause))
             (setq ,(into-var clause) ,form)
             (unless (realp ,(into-var clause))
               (error 'type-error :datum ,(into-var clause)
                                  :expected-type 'real)))
            (t
             (setq ,(into-var clause)
                   (min ,(into-var clause) ,form)))))))
