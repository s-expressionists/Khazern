(cl:in-package #:khazern)

;;; 6.1.3 Value Accumulation Clauses

(defclass accumulation-clause (selectable-clause accumulation-mixin form-mixin)
  ())

(defun parse-into (client scope tokens)
  (if (pop-token? client scope tokens '(eql :into))
      (pop-token client scope tokens 'symbol)
      (default-accumulation-variable)))

;;; COLLECT clause

(defclass collect-clause (accumulation-clause)
  ())

;;; COLLECT parsers

(defmethod parse-tokens (client (scope selectable-clauses) (keyword (eql :collect)) tokens)
  (make-instance 'collect-clause
                 :form (pop-token client scope tokens)
                 :var (make-instance 'd-spec
                                     :var-spec (parse-into client scope tokens)
                                     :accumulation-category 'list)))

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

;;; COLLECT expansion methods

(defmethod body-forms ((clause collect-clause))
  (let* ((form (form clause))
         (into-var (var-spec (var clause)))
         (tail-var (tail-variable into-var)))
    (when (and *it-var* (it-keyword-p form))
      (setf form *it-var*))
    `((if (consp ,tail-var)
          (rplacd ,tail-var (setq ,tail-var (cons ,form nil)))
          (setq ,tail-var (cons ,form nil)
                ,into-var ,tail-var)))))

;;; APPEND clause

(defclass append-clause (accumulation-clause)
  ())

;;; APPEND parsers

(defmethod parse-tokens (client (scope selectable-clauses) (keyword (eql :append)) tokens)
  (make-instance 'append-clause
                 :form (pop-token client scope tokens)
                 :var (make-instance 'd-spec
                                     :var-spec (parse-into client scope tokens)
                                     :accumulation-category 'list)))

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

;;; APPEND expansion methods

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

;;; NCONC clause

(defclass nconc-clause (accumulation-clause)
  ())

;;; NCONC parsers

(defmethod parse-tokens (client (scope selectable-clauses) (keyword (eql :nconc)) tokens)
  (make-instance 'nconc-clause
                 :form (pop-token client scope tokens)
                 :var (make-instance 'd-spec
                                     :var-spec (parse-into client scope tokens)
                                     :accumulation-category 'list)))

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

;;; NCONC expansion methods

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

;;; COUNT clause

(defclass count-clause (accumulation-clause)
  ())

;;; COUNT parsers

(defmethod parse-tokens (client (scope selectable-clauses) (keyword (eql :count)) tokens)
  (make-instance 'count-clause
                 :form (pop-token client scope tokens)
                 :var (make-instance 'd-spec
                                     :var-spec (parse-into client scope tokens)
                                     :type-spec (parse-type-spec client scope tokens 'fixnum)                          
                                     :accumulation-category 'count/sum)))

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
               'optional-type-spec/fixnum))

;;; COUNT expansion methods

(defmethod analyze ((clause count-clause))
  (check-subtype (type-spec (var clause)) 'number))

(defmethod body-forms ((clause count-clause))
  (let ((form (form clause))
        (into-var (var-spec (var clause))))
    `((when ,(if (and *it-var* (it-keyword-p form))
                 *it-var*
                 form)
        (setq ,into-var
              (1+ ,into-var))))))

;;; MINIMIZE/MAXIMIZE clause

(defclass extremum-clause (accumulation-clause)
  ((%reduce-function :reader reduce-function
                     :initarg :reduce-function)))

;;; MINIMIZE/MAXIMIZE parsers

(defmethod parse-tokens (client (scope selectable-clauses) (keyword (eql :minimize)) tokens)
  (make-instance 'extremum-clause
                 :form (pop-token client scope tokens)
                 :reduce-function 'min
                 :var (make-instance 'd-spec
                                     :var-spec (parse-into client scope tokens)
                                     :type-spec (parse-type-spec client scope tokens 'real)                          
                                     :accumulation-category 'max/min)))

(defmethod parse-tokens (client (scope selectable-clauses) (keyword (eql :maximize)) tokens)
  (make-instance 'extremum-clause
                 :form (pop-token client scope tokens)
                 :reduce-function 'max
                 :var (make-instance 'd-spec
                                     :var-spec (parse-into client scope tokens)
                                     :type-spec (parse-type-spec client scope tokens 'real)                          
                                     :accumulation-category 'max/min)))

;;; MINIMIZE/MAXIMIZE expansion methods

(defmethod analyze ((clause extremum-clause))
  (setf (type-spec (var clause)) (type-or-null (type-spec (var clause))))
  (check-subtype (type-spec (var clause)) '(or null real)))

(defmethod body-forms ((clause extremum-clause))
  (let ((form (form clause))
        (var (var-spec (var clause))))
    (when (and *it-var* (it-keyword-p form))
      (setf form *it-var*))
    `((setq ,var
            (if ,var
                (,(reduce-function clause) ,var ,form)
                ,form)))))

;;; SUM clause

(defclass sum-clause (accumulation-clause)
  ())

;;; SUM parsers

(defmethod parse-tokens (client (scope selectable-clauses) (keyword (eql :sum)) tokens)
  (make-instance 'sum-clause
                 :form (pop-token client scope tokens)
                 :var (make-instance 'd-spec
                                     :var-spec (parse-into client scope tokens)
                                     :type-spec (parse-type-spec client scope tokens 'number)                          
                                     :accumulation-category 'count/sum)))

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
               'optional-type-spec/number))

;;; SUM expansion methods

(defmethod analyze ((clause sum-clause))
  (check-subtype (type-spec (var clause)) 'number))

(defmethod body-forms ((clause sum-clause))
  (let ((form (form clause)))
    `((incf ,(var-spec (var clause))
            ,(if (and *it-var* (it-keyword-p form))
                 *it-var*
                 form)))))
