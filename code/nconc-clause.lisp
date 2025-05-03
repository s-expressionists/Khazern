(cl:in-package #:khazern)

(defclass nconc-clause (list-accumulation-clause form-mixin) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser nconc-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var)
                 (make-instance 'nconc-clause
                                :form form
                                :into-var var))
               (keyword :nconc :nconcing)
               'terminal
               'anything
               'optional-into-phrase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute body-forms.

(defmethod body-forms ((clause nconc-clause) end-tag)
  (declare (ignore end-tag))
  (let* ((form (form clause))
         (into-var (into-var clause))
         (tail-var (tail-variable into-var)))
    (when (and *it-var* (it-keyword-p form))
      (setf form *it-var*))
    `((tagbody
         (if (null ,into-var)
             (setq ,into-var ,form
                   ,tail-var ,into-var)
             (rplacd ,tail-var ,form))
       next
         (when (and (consp ,tail-var)
                    (consp (cdr ,tail-var)))
           (setq ,tail-var (cdr ,tail-var))
           (go next))))))
