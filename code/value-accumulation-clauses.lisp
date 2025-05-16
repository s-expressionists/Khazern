(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 6.1.3 Value Accumulation Clauses

(defclass accumulation-clause (selectable-clause accumulation-mixin form-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COLLECT clause

(defclass collect-clause (accumulation-clause)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser collect-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var)
                 (make-instance 'collect-clause
                                :form form
                                :var (make-instance 'd-spec
                                                    :var-spec var
                                                    :accumulation-category 'list)))
               (keyword :collect :collecting)
               'terminal
               'anything
               'optional-into-phrase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute body-forms.

(defmethod body-forms ((clause collect-clause))
  (let* ((form (form clause))
         (into-var (var-spec (var clause)))
         (tail-var (tail-variable into-var)))
    (when (and *it-var* (it-keyword-p form))
      (setf form *it-var*))
    `((cond ((null ,into-var)
             (setq ,tail-var (cons ,form nil)
                   ,into-var ,tail-var))
            (t
             (rplacd ,tail-var (cons ,form nil))
             (setq ,tail-var (cdr ,tail-var)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; APPEND clause

(defclass append-clause (accumulation-clause)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser append-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var)
                 (make-instance 'append-clause
                                :form form
                                :var (make-instance 'd-spec
                                                    :var-spec var
                                                    :accumulation-category 'list)))
               (keyword :append :appending)
               'terminal
               'anything
               'optional-into-phrase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute body-forms.

(defmethod body-forms ((clause append-clause))
  (let* ((head-var (gensym))
         (form (form clause))
         (into-var (var-spec (var clause)))
         (tail-var (tail-variable into-var)))
    (when (and *it-var* (it-keyword-p form))
      (setf form *it-var*))
    `((let ((,head-var ,form))
        (tagbody
         repeat
           (cond ((null ,head-var))
                 ((and (null ,into-var) (consp ,head-var))
                  (setq ,into-var (cons (car ,head-var) nil)
                        ,tail-var ,into-var
                        ,head-var (cdr ,head-var))
                  (go repeat))
                 ((null ,into-var)
                  (setq ,into-var ,head-var
                        ,tail-var ,into-var))
                 ((consp ,head-var)
                  (rplacd ,tail-var (cons (car ,head-var) nil))
                  (setq ,tail-var (cdr ,tail-var)
                        ,head-var (cdr ,head-var))
                  (go repeat))
                 (t
                  (rplacd ,tail-var ,head-var)
                  (setq ,tail-var (cdr ,tail-var)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NCONC clause

(defclass nconc-clause (accumulation-clause)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser nconc-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var)
                 (make-instance 'nconc-clause
                                :form form
                                :var (make-instance 'd-spec
                                                    :var-spec var
                                                    :accumulation-category 'list)))
               (keyword :nconc :nconcing)
               'terminal
               'anything
               'optional-into-phrase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute body-forms.

(defmethod body-forms ((clause nconc-clause))
  (let* ((form (form clause))
         (into-var (var-spec (var clause)))
         (tail-var (tail-variable into-var))
         (next-tag (gensym "NEXT")))
    (when (and *it-var* (it-keyword-p form))
      (setf form *it-var*))
    `((tagbody
         (if (null ,into-var)
             (setq ,into-var ,form
                   ,tail-var ,into-var)
             (rplacd ,tail-var ,form))
       ,next-tag
         (when (and (consp ,tail-var)
                    (consp (cdr ,tail-var)))
           (setq ,tail-var (cdr ,tail-var))
           (go ,next-tag))))))


(defclass count-clause (accumulation-clause)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser count-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var type-spec)
                 (make-instance 'count-clause
                                :form form
                                :var (make-instance 'd-spec
                                                    :var-spec var
                                                    :type-spec type-spec
                                                    :accumulation-category 'count/sum)))
               (keyword :count :counting)
               'terminal
               'anything
               'optional-into-phrase
               'optional-type-spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.

(defmethod body-forms ((clause count-clause))
  (let ((form (form clause))
        (into-var (var-spec (var clause))))
    `((when ,(if (and *it-var* (it-keyword-p form))
                 *it-var*
                 form)
        (setq ,into-var
              (1+ ,into-var))))))


(defclass maximize-clause (accumulation-clause)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser maximize-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var type-spec)
                 (make-instance 'maximize-clause
                                :form form
                                :var (make-instance 'd-spec
                                                    :var-spec var
                                                    :type-spec (type-or-null type-spec)
                                                    :accumulation-category 'max/min)))
               (keyword :maximize :maximizing)
               'terminal
               'anything
               'optional-into-phrase
               'optional-type-spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.

(defmethod body-forms ((clause maximize-clause))
  (let ((form (form clause)))
    (when (and *it-var* (it-keyword-p form))
      (setf form *it-var*))
    `((cond ((null ,(var-spec (var clause)))
             (setq ,(var-spec (var clause)) ,form)
             (unless (realp ,(var-spec (var clause)))
               (error 'type-error :datum ,(var-spec (var clause))
                                  :expected-type 'real)))
            (t
             (setq ,(var-spec (var clause))
                   (max ,(var-spec (var clause)) ,form)))))))


(defclass minimize-clause (accumulation-clause)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser minimize-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var type-spec)
                 (make-instance 'minimize-clause
                                :form form
                                :var (make-instance 'd-spec
                                                    :var-spec var
                                                    :type-spec (type-or-null type-spec)
                                                    :accumulation-category 'max/min)))
               (keyword :minimize :minimizing)
               'terminal
               'anything
               'optional-into-phrase
               'optional-type-spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.

(defmethod body-forms ((clause minimize-clause))
  (let ((form (form clause)))
    (when (and *it-var* (it-keyword-p form))
      (setf form *it-var*))
    `((cond ((null ,(var-spec (var clause)))
             (setq ,(var-spec (var clause)) ,form)
             (unless (realp ,(var-spec (var clause)))
               (error 'type-error :datum ,(var-spec (var clause))
                                  :expected-type 'real)))
            (t
             (setq ,(var-spec (var clause))
                   (min ,(var-spec (var clause)) ,form)))))))



(defclass sum-clause (accumulation-clause)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser sum-clause (:body-clause :selectable-clause)
  (consecutive (lambda (form var type-spec)
                 (make-instance 'sum-clause
                                :form form
                                :var (make-instance 'd-spec
                                                    :var-spec var
                                                    :type-spec type-spec
                                                    :accumulation-category 'count/sum)))
               (keyword :sum :summing)
               'terminal
               'anything
               'optional-into-phrase
               'optional-type-spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.

(defmethod body-forms ((clause sum-clause))
  (let ((form (form clause)))
    `((incf ,(var-spec (var clause))
            ,(if (and *it-var* (it-keyword-p form))
                 *it-var*
                 form)))))
