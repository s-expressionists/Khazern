(in-package #:khazern)

;;; We define a class that is the root class of all termination-test
;;; clauses.  Recall that a termination-test clause is a main clause,
;;; and that the HyperSpec defines TERMINATION-TEST as follows:
;;;
;;;   termination-test ::= while form |
;;;                        until form |
;;;                        repeat form |
;;;                        always form |
;;;                        never form |
;;;                        thereis form

(defclass termination-test-clause (main-clause)
  ())

(defclass boolean-termination-test-clause (termination-test-clause accumulation-mixin form-mixin)
  ())


(defclass repeat-clause (termination-test-clause var-mixin form-mixin)
  ()
  (:default-initargs :var (make-instance 'd-spec
                                         :var-spec (gensym "REPEAT")
                                         :type-spec 'fixnum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(defmethod parse-tokens
    (client (scope body-clauses) (keyword (eql :repeat)) tokens)
  (make-instance 'repeat-clause :form (pop-token tokens)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the bindings.

(defmethod initial-bindings ((clause repeat-clause))
  `((,(var-spec (var clause)) (max 0 (ceiling ,(form clause))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the declarations.

(defmethod initial-declarations ((clause repeat-clause))
  `((type ,(type-spec (var clause)) ,(var-spec (var clause)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the initial step forms.

(defmethod initial-step-forms ((clause repeat-clause))
  `((when (zerop ,(var-spec (var clause)))
      (go ,*epilogue-tag*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the subsequent step forms.

(defmethod subsequent-step-forms ((clause repeat-clause))
  `((when (zerop (decf ,(var-spec (var clause))))
      (go ,*epilogue-tag*))))



(defclass always-clause (boolean-termination-test-clause)
  ()
   (:default-initargs :var (make-instance 'd-spec
                                         :var-spec (default-accumulation-variable)
                                         :accumulation-category 'always/never)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(defmethod parse-tokens
    (client (scope body-clauses) (keyword (eql :always)) tokens)
  (make-instance 'always-clause :form (pop-token tokens)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the body-forms

(defmethod body-forms ((clause always-clause))
  `((unless ,(form clause)
      (return-from ,*loop-name* nil))))


(defclass never-clause (boolean-termination-test-clause)
  ()
  (:default-initargs :var (make-instance 'd-spec
                                         :var-spec (default-accumulation-variable)
                                         :accumulation-category 'always/never)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(defmethod parse-tokens
    (client (scope body-clauses) (keyword (eql :never)) tokens)
  (make-instance 'never-clause :form (pop-token tokens)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the body-forms

(defmethod body-forms ((clause never-clause))
  `((when ,(form clause)
     (return-from ,*loop-name* nil))))


(defclass thereis-clause (boolean-termination-test-clause)
  ()
  (:default-initargs :var (make-instance 'd-spec
                                         :var-spec (default-accumulation-variable)
                                         :accumulation-category 'thereis)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(defmethod parse-tokens
    (client (scope body-clauses) (keyword (eql :thereis)) tokens)
  (make-instance 'thereis-clause :form (pop-token tokens)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the body-forms

(defmethod body-forms ((clause thereis-clause))
  (let ((var (var-spec (var clause))))
    `((when (setq ,var ,(form clause))
        (return-from ,*loop-name* ,var)))))


(defclass while-clause (termination-test-clause form-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(defmethod parse-tokens
    (client (scope body-clauses) (keyword (eql :while)) tokens)
  (make-instance 'while-clause :form (pop-token tokens)))

(defmethod parse-tokens
    (client (scope body-clauses) (keyword (eql :until)) tokens)
  (make-instance 'while-clause :form `(not ,(pop-token tokens))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the body-forms

(defmethod body-forms ((clause while-clause))
  `((unless ,(form clause)
      (go ,*epilogue-tag*))))
