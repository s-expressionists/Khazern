(cl:in-package #:khazern)

(defclass collect-clause (list-accumulation-clause) ())

(defclass collect-form-clause (collect-clause form-mixin)
  ())

(defclass collect-it-clause (collect-form-clause it-mixin)
  ())

(defclass collect-form-into-clause (into-mixin collect-clause form-mixin)
  ())

(defclass collect-it-into-clause (collect-form-into-clause it-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser collect-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var
                        &aux (itp (it-keyword-p form)))
                 (cond ((and itp var)
                        (make-instance 'collect-it-into-clause
                                       :form form
                                       :into-var var))
                       (itp
                        (make-instance 'collect-it-clause
                                       :form form))
                       (var
                        (make-instance 'collect-form-into-clause
                                       :form form
                                       :into-var var))
                       (t
                        (make-instance 'collect-form-clause
                                       :form form))))
               (keyword :collect :collecting)
               'terminal
               'anything
               'optional-into-phrase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute body-form.

(defun collect-clause-expander
    (form accumulation-variable list-tail-accumulation-variable)
  `(if (null ,accumulation-variable)
       ;; If the accumulation variable is NIL, then so is the tail
       ;; variable.  Then just allocate a new CONS cell containing the
       ;; value of the form, and set bot the accumulation variable and
       ;; the tail variable to point to it.
       (progn (setq ,list-tail-accumulation-variable
                    (cons ,form nil))
              (setq ,accumulation-variable
                    ,list-tail-accumulation-variable))
       ;; If the accumulation variable is not NIL, then the tail
       ;; variable may or may not be NIL.
       (progn
         ,(copy-cons-cells
           accumulation-variable list-tail-accumulation-variable)
         ;; When we come here, every CONS cell after the one that the
         ;; tail variable points to has been copied, and the tail
         ;; variable points to the last CONS cell in the list.  It
         ;; remains to attach a new CONS cell containing the value of
         ;; the form and to advance the tail variable to point to that
         ;; new CONS cell.
         (if (null (cdr ,list-tail-accumulation-variable))
             (progn (setf (cdr ,list-tail-accumulation-variable)
                          (cons ,form nil))
                    (setf ,list-tail-accumulation-variable
                          (cdr ,list-tail-accumulation-variable)))
             (error 'type-error
                    :datum (cdr ,list-tail-accumulation-variable)
                    :expected-type 'null)))))

(defmethod body-form ((clause collect-form-clause) end-tag)
  (declare (ignore end-tag))
  (collect-clause-expander
   (form clause) *accumulation-variable* *list-tail-accumulation-variable*))

(defmethod body-form ((clause collect-form-into-clause) end-tag)
  (declare (ignore end-tag))
  (collect-clause-expander
   (form clause) (into-var clause) (tail-variable (into-var clause))))

(defmethod body-form ((clause collect-it-clause) end-tag)
  (declare (ignore end-tag))
  (if *it-var*
      (collect-clause-expander
       *it-var*  *accumulation-variable* *list-tail-accumulation-variable*)
      (call-next-method)))
    
(defmethod body-form ((clause collect-it-into-clause) end-tag)
  (declare (ignore end-tag))
  (if *it-var*
      (collect-clause-expander
       *it-var* (into-var clause) (tail-variable (into-var clause)))
      (call-next-method)))
