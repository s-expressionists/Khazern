(in-package #:khazern)

;;; We define a class that is the root class of all termination-test
;;; clauses.  Recall that a termination-test clause is a main clause,
;;; and that the HyperSpec defines TERMINATION-TEST as follows:
;;;
;;;   termination-test ::= WHILE form | UNTIL form | REPEAT form | ALWAYS form | NEVER form |
;;;                        THEREIS form

(defclass termination-test-clause (body-clause)
  ())

(defmethod (setf clause-group) :after ((group (eql :variable)) (clause termination-test-clause))
  (warn 'possible-invalid-clause-order
        :clause (subseq *body* (start clause) (end clause))
        :found-group group
        :expected-group :main))

(defclass boolean-termination-test-clause
    (termination-test-clause accumulation-mixin form-mixin)
  ())

(defclass every-accumulation-clause (var-mixin)
  ())

(defmethod make-accumulation-clause (name type (category (eql :every)))
  (make-instance 'every-accumulation-clause
                 :var (make-instance 'simple-binding
                                     :var-spec name
                                     :type-spec type
                                     :accumulation-category category)))

(defmethod initial-bindings nconc ((instance every-accumulation-clause))
  (d-spec-simple-bindings (var instance) t))

(defmethod initial-declarations nconc ((instance every-accumulation-clause))
  (d-spec-outer-declarations (var instance)))

(defclass some-accumulation-clause (var-mixin)
  ())

(defmethod make-accumulation-clause (name type (category (eql :some)))
  (make-instance 'summation-accumulation-clause
                 :var (make-instance 'simple-binding
                                     :var-spec name
                                     :type-spec type
                                     :accumulation-category category)))

(defmethod initial-bindings nconc ((instance some-accumulation-clause))
  (d-spec-outer-bindings (var instance)))

(defmethod initial-declarations nconc ((instance some-accumulation-clause))
  (d-spec-outer-declarations (var instance)))

(defclass repeat-clause (termination-test-clause)
  ((%count-ref :accessor count-ref)))

(defmethod parse-clause
    (client (scope extended-superclause) (keyword (eql :repeat)))
  (let ((form (pop-token))
        (instance (make-instance 'repeat-clause
                                 :start *start*
                                 :end *index*)))
    (setf (count-ref instance) (add-simple-binding instance
                                                   :var "REPEAT"
                                                   :type 'fixnum
                                                   :form (if (numberp form)
                                                             (max 0 (ceiling form))
                                                             `(max 0 (ceiling ,form)))))
    instance))

(defun expand-repeat (clause group)
  (when (eq (clause-group clause) group)
    `((when (minusp (decf ,(count-ref clause)))
        (go ,*epilogue-tag*)))))

(defmethod body-forms ((clause repeat-clause))
  (expand-repeat clause :main))

(defmethod initial-step-forms ((clause repeat-clause))
  (expand-repeat clause :variable))

(defmethod subsequent-step-forms ((clause repeat-clause))
  (expand-repeat clause :variable))

(defclass always-clause (boolean-termination-test-clause)
  ()
  (:default-initargs :var (make-instance 'simple-binding
                                         :var-spec (default-accumulation-variable)
                                         :accumulation-category :every)))

(defmethod parse-clause
    (client (scope extended-superclause) (keyword (eql :always)))
  (make-instance 'always-clause
                 :start *start*
                 :form (pop-token)
                 :end *index*))

(defun expand-always (clause group)
  (when (eq (clause-group clause) group)
    `((unless ,(form clause)
        (return-from ,*loop-name* nil)))))
    
(defmethod body-forms ((clause always-clause))
  (expand-always clause :main))
    
(defmethod initial-step-forms ((clause always-clause))
  (expand-always clause :variable))
    
(defmethod subsequent-step-forms ((clause always-clause))
  (expand-always clause :variable))

(defclass never-clause (boolean-termination-test-clause)
  ()
  (:default-initargs :var (make-instance 'simple-binding
                                         :var-spec (default-accumulation-variable)
                                         :accumulation-category :every)))

(defmethod parse-clause
    (client (scope extended-superclause) (keyword (eql :never)))
  (make-instance 'never-clause 
                 :start *start*
                 :form (pop-token)
                 :end *index*))

(defun expand-never (clause group)
  (when (eq (clause-group clause) group)
    `((when ,(form clause)
        (return-from ,*loop-name* nil)))))

(defmethod body-forms ((clause never-clause))
  (expand-never clause :main))

(defmethod initial-step-forms ((clause never-clause))
  (expand-never clause :variable))

(defmethod subsequent-step-forms ((clause never-clause))
  (expand-never clause :variable))

(defclass thereis-clause (boolean-termination-test-clause)
  ()
  (:default-initargs :var (make-instance 'simple-binding
                                         :var-spec (default-accumulation-variable)
                                         :accumulation-category :some)))

(defmethod parse-clause
    (client (scope extended-superclause) (keyword (eql :thereis)))
  (make-instance 'thereis-clause
                 :start *start*
                 :form (pop-token)
                 :end *index*))

(defun expand-thereis (clause group)
  (when (eq (clause-group clause) group)
    (let ((var (var-spec (var clause))))
      `((when (setq ,var ,(form clause))
          (return-from ,*loop-name* ,var))))))

(defmethod body-forms ((clause thereis-clause))
  (expand-thereis clause :main))

(defmethod initial-step-forms ((clause thereis-clause))
  (expand-thereis clause :variable))

(defmethod subsequent-step-forms ((clause thereis-clause))
  (expand-thereis clause :variable))

(defclass while-clause (termination-test-clause form-mixin)
  ())

(defmethod parse-clause
    (client (scope extended-superclause) (keyword (eql :while)))
  (make-instance 'while-clause
                 :start *start*
                 :form (pop-token)
                 :end *index*))

(defmethod parse-clause
    (client (scope extended-superclause) (keyword (eql :until)))
  (make-instance 'while-clause
                 :start *start*
                 :form `(not ,(pop-token))
                 :end *index*))

(defun expand-while (clause group)
  (when (eq (clause-group clause) group)
    `((unless ,(form clause)
        (go ,*epilogue-tag*)))))

(defmethod body-forms ((clause while-clause))
  (expand-while clause :main))

(defmethod initial-step-forms ((clause while-clause))
  (expand-while clause :variable))

(defmethod subsequent-step-forms ((clause while-clause))
  (expand-while clause :variable))
