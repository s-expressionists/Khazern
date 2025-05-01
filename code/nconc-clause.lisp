(cl:in-package #:khazern)

(defclass nconc-clause (list-accumulation-clause form-mixin) ())

(defclass nconc-it-clause (nconc-form-clause it-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser nconc-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var
                        &aux (itp (it-keyword-p form)))
                 (cond ((and itp var)
                        (make-instance 'nconc-it-clause
                                       :form form
                                       :into-var var))
                       (itp
                        (make-instance 'nconc-it-clause
                                       :form form))
                       (var
                        (make-instance 'nconc-clause
                                       :form form
                                       :into-var var))
                       (t
                        (make-instance 'nconc-clause
                                       :form form))))
               (keyword :nconc :nconcing)
               'terminal
               'anything
               'optional-into-phrase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute body-forms.

(defun nconc-clause-expander
    (form accumulation-variable list-tail-accumulation-variable)
  `((cond ((null ,accumulation-variable)
           ;; If the accumulation variable is NIL, then so is the tail
           ;; variable.  We first set the accumulation variable to the
           ;; value of the form.  Then we make the tail variable point to
           ;; the last cell of the list.
           (setq ,accumulation-variable ,form
                 ,list-tail-accumulation-variable
                 (last ,accumulation-variable)))
          (t
           ;; If the accumulation variable is not NIL, then the tail
           ;; variable may or may not be NIL.
           ,(copy-cons-cells
             accumulation-variable list-tail-accumulation-variable)
           ;; When we come here, every CONS cell after the one that the
           ;; tail variable points to has been copied, and the tail
           ;; variable points to the last CONS cell in the list.  It
           ;; remains to attach the new list to the end, and to set the
           ;; tail variable to point to the last cell of the newly
           ;; attached list.
           (setf (cdr ,list-tail-accumulation-variable)
                 ,form
                 ,list-tail-accumulation-variable
                 (last ,list-tail-accumulation-variable))))))

(defmethod body-forms ((clause nconc-clause) end-tag)
  (declare (ignore end-tag))
  (nconc-clause-expander
   (form clause) (into-var clause) (tail-variable (into-var clause))))

(defmethod body-forms ((clause nconc-it-clause) end-tag)
  (declare (ignore end-tag))
  (if *it-var*
      (nconc-clause-expander
       *it-var* (into-var clause) (tail-variable (into-var clause)))
      (call-next-method)))
