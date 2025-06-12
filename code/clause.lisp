(cl:in-package #:khazern)

;;;; The terminology used here is that of the BNF grammar in the
;;;; dictionary description of the loop macro in the HyperSpec.  It is
;;;; not the same as the terminology used in the section 6.1.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Common classes.

(defclass clause ()
  ((%start :accessor start
           :initarg :start
           :type fixnum)
   (%end :accessor end
         :initarg :end
         :type fixnum)))

;;;  Clauses that accept `AND'.
(defclass simple-superclause (clause)
  ((%subclauses :accessor subclauses
                :initarg :subclauses
                :initform nil)))

(defmethod analyze :before ((clause simple-superclause))
  (mapc #'analyze (subclauses clause)))

(defmethod map-variables (function (clause simple-superclause))
  (map-variables function (subclauses clause)))

(defmethod prologue-forms ((clause simple-superclause))
  (mapcan #'prologue-forms (subclauses clause)))

(defmethod body-forms ((clause simple-superclause))
  (mapcan #'body-forms (subclauses clause)))

(defmethod epilogue-forms ((clause simple-superclause))
  (mapcan #'epilogue-forms (subclauses clause)))

(defmethod name ((clause simple-superclause))
  (some #'name (subclauses clause)))

(defclass sequential-superclause (simple-superclause)
  ())

(defmethod initial-step-forms ((clause sequential-superclause))
  (mapcan (lambda (clause)
            (wrap-let* (initial-step-bindings clause)
                       (initial-step-declarations clause)
                       (initial-step-forms clause)))
          (subclauses clause)))

(defmethod subsequent-step-forms ((clause sequential-superclause))
  (mapcan (lambda (clause)
            (wrap-let* (subsequent-step-bindings clause)
                       (subsequent-step-declarations clause)
                       (subsequent-step-forms clause)))
          (subclauses clause)))

(defmethod wrap-forms ((clause sequential-superclause) forms)
  (wrap-let* (mapcan #'initial-bindings (subclauses clause))
             (mapcan #'initial-declarations (subclauses clause))
             (reduce #'wrap-forms (subclauses clause)
                     :from-end t :initial-value forms)))

(defclass parallel-superclause (simple-superclause)
  ())

(defmethod initial-step-forms ((clause parallel-superclause))
  (wrap-let* (mapcan #'initial-step-bindings (subclauses clause))
             (mapcan #'initial-step-declarations (subclauses clause))
             (mapcan #'initial-step-forms (subclauses clause))))

(defmethod subsequent-step-forms ((clause parallel-superclause))
  (wrap-let* (mapcan #'subsequent-step-bindings (subclauses clause))
             (mapcan #'subsequent-step-declarations (subclauses clause))
             (mapcan #'subsequent-step-forms (subclauses clause))))

(defmethod wrap-forms ((clause parallel-superclause) forms)
  (wrap-let (mapcan #'initial-bindings (subclauses clause))
            (mapcan #'initial-declarations (subclauses clause))
            (reduce #'wrap-forms (subclauses clause)
                    :from-end t :initial-value forms)))

(defclass extended-superclause (selectable-superclause sequential-superclause)
  ())

(defclass var-mixin ()
  ((%var :accessor var
         :initarg :var)))

(defmethod map-variables (function (clause var-mixin))
  (map-variables function (var clause)))

;;; Mixin for clauses that take a list of compound forms.
(defclass compound-forms-mixin ()
  ((%forms :accessor forms
           :initarg :forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin for clauses that has an explicit form argument.

(defclass form-mixin ()
  ((%form :accessor form
          :initarg :form)))

(defclass form-var-mixin ()
  ((%form-var :accessor form-var
              :initform (gensym "FORM"))))

(defclass accumulation-mixin (var-mixin)
  ()
  (:default-initargs :var (make-instance 'd-spec
                                         :var-spec (default-accumulation-variable)
                                         :accumulation-category nil)))

(defmethod initialize-instance :after ((instance accumulation-mixin) &rest initargs &key)
  (declare (ignore initargs))
  (unless (var-spec (var instance))
    (setf (var-spec (var instance)) (default-accumulation-variable))))

;;; In the dictionary entry for LOOP, the HyperSpec says:
;;;
;;;   variable-clause ::= with-clause | initial-final | for-as-clause
;;;
;;; Here, we exclude initial-final.  The reason for that is that
;;; initial-final is also one of the possibilities for a
;;; main-clause, and the reason for this "multiple inheritance" is
;;; so that the LOOP macro syntax can be defined to have the syntax:
;;;
;;;   loop [name-clause] {variable-clause}* {main-clause}*
;;;
;;; which then allows for INITIALLY and FINALLY clauses to occur
;;; anywhere after the name-clause.
;;;
;;; What we do here is to treat INITIALLY and FINALLY specially, so
;;; that they are neither main clauses nor variable clauses.
;;; Therefore, here, we have:
;;;
;;;   variable-clause ::= with-clause | for-as-clause

(defclass variable-clause (clause)
  ())

(defmethod clause-group ((clause variable-clause))
  :variable)

(defmethod (setf clause-group) (group (clause variable-clause))
  (if (eq group :variable)
      group
      (error 'invalid-clause-order
             :clause (subseq *body* (start clause) (end clause))
             :found-group group
             :expected-group :variable)))

;;; In the dictionary entry for LOOP, the HyperSpec says:
;;;
;;;   main-clause ::= unconditional | 
;;;                   accumulation |
;;;                   conditional |
;;;                   termination-test |
;;;                   initial-final
;;;
;;; Here, we exclude initial-final.  The reason for that is that
;;; initial-final is also one of the possibilities for a
;;; variable-clause, and the reason for this "multiple inheritance" is
;;; so that the LOOP macro syntax can be defined to have the syntax:
;;;
;;;   loop [name-clause] {variable-clause}* {main-clause}*
;;;
;;; which then allows for INITIALLY and FINALLY clauses to occur
;;; anywhere after the name-clause.
;;;
;;; What we do here is to treat INITIALLY and FINALLY specially, so
;;; that they are neither main clauses nor variable clauses.
;;; Therefore, here, we have:
;;;
;;;   main-clause ::= unconditional | 
;;;                   accumulation |
;;;                   conditional |
;;;                   termination-test
;;;
;;; Furthermore, the HyperSpec defines selectable-clause like this:
;;;
;;;   selectable-clause ::= unconditional | accumulation | conditional 
;;;
;;; so we can say:
;;;
;;;    main-clause ::= selectable-clause | termination-test

(defclass main-clause (clause)
  ())

(defmethod clause-group ((clause main-clause))
  :main)

(defmethod (setf clause-group) (group (clause main-clause))
  (if (eq group :main)
      group
      (error 'invalid-clause-order
             :clause (subseq *body* (start clause) (end clause))
             :found-group group
             :expected-group :main)))

;;; Recall that in the dictionary entry for LOOP, the HyperSpec says:
;;;
;;;   main-clause ::= unconditional | 
;;;                   accumulation |
;;;                   conditional |
;;;                   termination-test |
;;;                   initial-final
;;;
;;; Though here, we exclude initial-final so that we have:
;;;
;;;   main-clause ::= unconditional | 
;;;                   accumulation |
;;;                   conditional |
;;;                   termination-test
;;;
;;; Furthermore, the HyperSpec defines selectable-clause like this:
;;;
;;;   selectable-clause ::= unconditional | accumulation | conditional 
;;;
;;; so we can say:
;;;
;;;    main-clause ::= selectable-clause | termination-test

(defclass selectable-clause (main-clause)
  ())

(defclass body-clause (clause)
  ((%clause-group :accessor clause-group
                  :initform :main)))

(defclass selectable-superclause (selectable-clause)
  ())

