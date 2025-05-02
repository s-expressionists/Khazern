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
    `((cond ((null ,into-var)
             ;; If the accumulation variable is NIL, then so is the tail
             ;; variable.  We first set the accumulation variable to the
             ;; value of the form.  Then we make the tail variable point to
             ;; the last cell of the list.
             (setq ,into-var ,form
                   ,tail-var (last ,into-var)))
            (t
             ;; If the accumulation variable is not NIL, then the tail
             ;; variable may or may not be NIL.
             ,(copy-cons-cells into-var tail-var)
             ;; When we come here, every CONS cell after the one that the
             ;; tail variable points to has been copied, and the tail
             ;; variable points to the last CONS cell in the list.  It
             ;; remains to attach the new list to the end, and to set the
             ;; tail variable to point to the last cell of the newly
             ;; attached list.
             (setf (cdr ,tail-var)
                   ,form
                   ,tail-var
                   (last ,tail-var)))))))
