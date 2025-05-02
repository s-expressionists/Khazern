(cl:in-package #:khazern)

(defclass append-clause (list-accumulation-clause form-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser append-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var)
                 (make-instance 'append-clause
                                :form form
                                :into-var var))
               (keyword :append :appending)
               'terminal
               'anything
               'optional-into-phrase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute body-forms.

(defmethod body-forms ((clause append-clause) end-tag)
  (declare (ignore end-tag))
  (let* ((form (form clause))
         (into-var (into-var clause))
         (tail-var (tail-variable into-var)))
    (when (and *it-var* (it-keyword-p form))
      (setf form *it-var*))
    `((cond ((null ,into-var)
             ;; If the accumulation variable is NIL, then so is the tail
             ;; variable.  We leave the tail variable as NIL so as to
             ;; indicate that every CONS cell in the list starting at the
             ;; accumulation variable must be copied whenever yet more CONS
             ;; cells are attached at the end.
             (setq ,into-var ,form))
            (t
             ;; If the accumulation variable is not NIL, then the tail
             ;; variable may or may not be NIL.
             ,(copy-cons-cells into-var tail-var)
             ;; When we come here, every CONS cell after the one that the
             ;; tail variable points to has been copied, and the tail
             ;; variable points to the last CONS cell in the list.  It
             ;; remains to attach the new list to the end.  And we leave
             ;; the tail variable where it is, indicating that the CONS
             ;; cells of the newly attached list must be copied whenever
             ;; yet more cells are attached at the end.
             (unless (null (cdr ,tail-var))
               (error 'type-error
                      :datum (cdr ,tail-var)
                      :expected-type 'null))
             (setf (cdr ,tail-var)
                   ,form))))))
