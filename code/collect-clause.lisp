(cl:in-package #:khazern)

(defclass collect-clause (list-accumulation-clause form-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser collect-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var)
                 (make-instance 'collect-clause
                                :form form
                                :into-var var))
               (keyword :collect :collecting)
               'terminal
               'anything
               'optional-into-phrase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute body-forms.

(defmethod body-forms ((clause collect-clause) end-tag)
  (declare (ignore end-tag))
  (let* ((form (form clause))
         (into-var (into-var clause))
         (tail-var (tail-variable into-var)))
    (when (and *it-var* (it-keyword-p form))
      (setf form *it-var*))
    `((cond ((null ,into-var)
             (setq ,tail-var (cons ,form nil)
                   ,into-var ,tail-var))
            (t
             (tagbody
              next
                (when (consp (cdr ,tail-var))
                  (setq ,tail-var (cdr ,tail-var))
                  (go next)))
             (if (cdr ,tail-var)
                 (error "improper list")
                 (setf (cdr ,tail-var) (cons ,form nil)
                       ,tail-var (cdr ,tail-var))))))))
