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
             ;; If the accumulation variable is NIL, then so is the tail
             ;; variable.  Then just allocate a new CONS cell containing the
             ;; value of the form, and set bot the accumulation variable and
             ;; the tail variable to point to it.
             (setq ,tail-var (cons ,form nil)
                   ,into-var ,tail-var))
            ;; If the accumulation variable is not NIL, then the tail
            ;; variable may or may not be NIL.
            (t
             ,(copy-cons-cells
               into-var tail-var)
             ;; When we come here, every CONS cell after the one that the
             ;; tail variable points to has been copied, and the tail
             ;; variable points to the last CONS cell in the list.  It
             ;; remains to attach a new CONS cell containing the value of
             ;; the form and to advance the tail variable to point to that
             ;; new CONS cell.
             (unless (null (cdr ,tail-var))
               (error 'type-error
                      :datum (cdr ,tail-var)
                      :expected-type 'null))
             (setf (cdr ,tail-var)
                   (cons ,form nil)
                   ,tail-var
                   (cdr ,tail-var)))))))
