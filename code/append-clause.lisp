(cl:in-package #:khazern)

(defclass append-clause (list-accumulation-clause) ())

(defclass append-form-clause (append-clause form-mixin)
  ())

(defclass append-it-clause (append-form-clause it-mixin)
  ())

(defclass append-form-into-clause (into-mixin append-clause form-mixin)
  ())

(defclass append-it-into-clause (append-form-into-clause it-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser append-it-into-clause ()
  (consecutive (lambda (append it into var)
                 (declare (ignore append into))
                 (make-instance 'append-it-into-clause
                   :form it
                   :into-var var))
               (keyword 'append 'appending)
               (keyword 'it)
               (keyword 'into)
               (singleton #'identity
                          (lambda (x)
                            (and (symbolp x) (not (constantp x)))))))

(define-parser append-it-clause ()
  (consecutive (lambda (append it)
                 (declare (ignore append))
                 (make-instance 'append-it-clause
                   :form it))
               (keyword 'append 'appending)
               (keyword 'it)))

(define-parser append-form-into-clause ()
  (consecutive (lambda (append form into var)
                 (declare (ignore append into))
                 (make-instance 'append-form-into-clause
                   :form form
                   :into-var var))
               (keyword 'append 'appending)
               'anything
               (keyword 'into)
               (singleton #'identity
                          (lambda (x)
                            (and (symbolp x) (not (constantp x)))))))

(define-parser append-form-clause ()
  (consecutive (lambda (append form)
                 (declare (ignore append))
                 (make-instance 'append-form-clause
                   :form form))
               (keyword 'append 'appending)
               'anything))

(define-parser append-clause (:body-clause :selectable-clause)
  (alternative 'append-it-into-clause
               'append-it-clause
               'append-form-into-clause
               'append-form-clause))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute body-form.

(defun append-clause-expander
    (form accumulation-variable list-tail-accumulation-variable)
  `(if (null ,accumulation-variable)
       ;; If the accumulation variable is NIL, then so is the tail
       ;; variable.  We leave the tail variable as NIL so as to
       ;; indicate that every CONS cell in the list starting at the
       ;; accumulation variable must be copied whenever yet more CONS
       ;; cells are attached at the end.
       (setq ,accumulation-variable ,form)
       ;; If the accumulation variable is not NIL, then the tail
       ;; variable may or may not be NIL.
       (progn
         ,(copy-cons-cells
           accumulation-variable list-tail-accumulation-variable)
         ;; When we come here, every CONS cell after the one that the
         ;; tail variable points to has been copied, and the tail
         ;; variable points to the last CONS cell in the list.  It
         ;; remains to attach the new list to the end.  And we leave
         ;; the tail variable where it is, indicating that the CONS
         ;; cells of the newly attached list must be copied whenever
         ;; yet more cells are attached at the end.
         (if (null (cdr ,list-tail-accumulation-variable))
             (setf (cdr ,list-tail-accumulation-variable)
                   ,form)
             (error 'type-error
                    :datum (cdr ,list-tail-accumulation-variable)
                    :expected-type 'null)))))

(defmethod body-form ((clause append-form-clause) end-tag)
  (declare (ignore end-tag))
  (append-clause-expander
   (form clause) *accumulation-variable* *list-tail-accumulation-variable*))

(defmethod body-form ((clause append-form-into-clause) end-tag)
  (declare (ignore end-tag))
  (append-clause-expander
   (form clause) (into-var clause) (tail-variable (into-var clause))))

(defmethod body-form ((clause append-it-clause) end-tag)
  (declare (ignore end-tag))
  (if *it-var*
      (append-clause-expander
       *it-var*  *accumulation-variable* *list-tail-accumulation-variable*)
      (call-next-method)))

(defmethod body-form ((clause append-it-into-clause) end-tag)
  (declare (ignore end-tag))
  (if *it-var*
      (append-clause-expander
       *it-var* (into-var clause) (tail-variable (into-var clause)))
      (call-next-method)))
