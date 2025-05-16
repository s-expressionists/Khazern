(cl:in-package #:khazern)

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

(define-parser repeat-clause (:body-clause)
  (consecutive (lambda (form)
                 (make-instance 'repeat-clause :form form))
               (keyword :repeat)
               'terminal
               'anything))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the bindings.

(defmethod initial-bindings ((clause repeat-clause))
  `((,(var-spec (var clause)) (max 0 (ceiling ,(form clause))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the declarations.

(defmethod initial-declarations ((clause repeat-clause))
  `((cl:type ,(type-spec (var clause)) ,(var-spec (var clause)))))

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

(define-parser always-clause (:body-clause)
  (consecutive (lambda (form)
                 (make-instance 'always-clause
                   :form form))
               (keyword :always)
               'terminal
               'anything))

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

(define-parser never-clause (:body-clause)
  (consecutive (lambda (form)
                 (make-instance 'never-clause :form form))
               (keyword :never)
               'terminal
               'anything))

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

(define-parser thereis-clause (:body-clause)
  (consecutive (lambda (form)
                 (make-instance 'thereis-clause :form form))
               (keyword :thereis)
               'terminal
               'anything))

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

(define-parser while-clause (:body-clause)
  (consecutive (lambda (form)
                 (make-instance 'while-clause :form form))
               (keyword :while)
               'terminal
               'anything))

(define-parser until-clause (:body-clause)
  (consecutive (lambda (form)
                 (make-instance 'while-clause :form `(not ,form)))
               (keyword :until)
               'terminal
               'anything))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the body-forms

(defmethod body-forms ((clause while-clause))
  `((unless ,(form clause)
      (go ,*epilogue-tag*))))
