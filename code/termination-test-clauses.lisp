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

(defclass every-accumulation-clause (clause)
  ())

(defmethod make-accumulation-scope ((client standard-client) name type (category (eql :every)) references)
  (declare (ignore references))
  (let ((instance (make-instance 'every-accumulation-clause)))
    (add-simple-binding instance :var name :type type :accumulation-category category :form t)
    instance))

(defclass some-accumulation-clause (clause)
  ())

(defmethod make-accumulation-scope ((client standard-client) name type (category (eql :some)) references)
  (declare (ignore references))
  (let ((instance (make-instance 'some-accumulation-clause)))
    (add-simple-binding instance :var name :type type :accumulation-category category :form nil)
    instance))

(defclass repeat-clause (termination-test-clause)
  ((%count-ref :accessor count-ref)))

(defmethod parse-clause
    (client (region body-region) (keyword (eql :repeat)) &key)
  (let ((form (parse-token))
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

(defmethod step-intro-forms ((clause repeat-clause) initialp)
  (declare (ignore initialp))
  (expand-repeat clause :variable))

(defclass always-clause (boolean-termination-test-clause)
  ()
  (:default-initargs :accum-var (make-instance 'simple-binding
                                               :var-spec (default-accumulation-variable)
                                               :accumulation-category :every)))

(defmethod parse-clause
    (client (region body-region) (keyword (eql :always)) &key)
  (make-instance 'always-clause
                 :start *start*
                 :form (parse-token)
                 :end *index*))

(defun expand-always (clause group)
  (when (eq (clause-group clause) group)
    `((unless ,(form clause)
        (return-from ,*loop-name* nil)))))
    
(defmethod body-forms ((clause always-clause))
  (expand-always clause :main))
    
(defmethod step-intro-forms ((clause always-clause) initialp)
  (declare (ignore initialp))
  (expand-always clause :variable))

(defclass never-clause (boolean-termination-test-clause)
  ()
  (:default-initargs :accum-var (make-instance 'simple-binding
                                               :var-spec (default-accumulation-variable)
                                               :accumulation-category :every)))

(defmethod parse-clause
    (client (region body-region) (keyword (eql :never)) &key)
  (make-instance 'never-clause 
                 :start *start*
                 :form (parse-token)
                 :end *index*))

(defun expand-never (clause group)
  (when (eq (clause-group clause) group)
    `((when ,(form clause)
        (return-from ,*loop-name* nil)))))

(defmethod body-forms ((clause never-clause))
  (expand-never clause :main))

(defmethod step-intro-forms ((clause never-clause) initialp)
  (declare (ignore initialp))
  (expand-never clause :variable))

(defclass thereis-clause (boolean-termination-test-clause)
  ()
  (:default-initargs :accum-var (make-instance 'simple-binding
                                               :var-spec (default-accumulation-variable)
                                               :accumulation-category :some)))

(defmethod parse-clause
    (client (region body-region) (keyword (eql :thereis)) &key)
  (make-instance 'thereis-clause
                 :start *start*
                 :form (parse-token)
                 :end *index*))

(defun expand-thereis (clause group)
  (when (eq (clause-group clause) group)
    (let ((var (var-spec (accum-var clause))))
      `((when (setq ,var ,(form clause))
          (return-from ,*loop-name* ,var))))))

(defmethod body-forms ((clause thereis-clause))
  (expand-thereis clause :main))

(defmethod step-intro-forms ((clause thereis-clause) initialp)
  (declare (ignore initialp))
  (expand-thereis clause :variable))

(defclass while-clause (termination-test-clause form-mixin)
  ())

(defmethod parse-clause
    (client (region body-region) (keyword (eql :while)) &key)
  (make-instance 'while-clause
                 :start *start*
                 :form (parse-token)
                 :end *index*))

(defmethod parse-clause
    (client (region body-region) (keyword (eql :until)) &key)
  (make-instance 'while-clause
                 :start *start*
                 :form `(not ,(parse-token))
                 :end *index*))

(defun expand-while (clause group)
  (when (eq (clause-group clause) group)
    `((unless ,(form clause)
        (go ,*epilogue-tag*)))))

(defmethod body-forms ((clause while-clause))
  (expand-while clause :main))

(defmethod step-intro-forms ((clause while-clause) initialp)
  (declare (ignore initialp))
  (expand-while clause :variable))
