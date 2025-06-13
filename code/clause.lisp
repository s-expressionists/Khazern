(cl:in-package #:khazern)

;;;; Clause classes and common methods
;;;;
;;;; The terminology used here is that of the BNF grammar in the dictionary description of the
;;;; loop macro in the HyperSpec.  It is not the same as the terminology used in the section
;;;; 6.1.
;;;;
;;;; In the dictionary entry for LOOP, the HyperSpec says:
;;;;
;;;; loop [name-clause] {variable-clause}* {main-clause}*
;;;;   variable-clause   ::= with-clause | initial-final | for-as-clause
;;;;   main-clause       ::= unconditional | accumulation | conditional | termination-test |
;;;;                         initial-final
;;;;   selectable-clause ::= unconditional | accumulation | conditional
;;;;
;;;; Since initial-final can be either a variable-clause or a main-clause we introduce new
;;;; clause groups called body-clause and binding-clause so the overall BNF of LOOP becomes:
;;;;
;;;; loop [name-clause] {variable-clause}* {main-clause}*
;;;;   variable-clause   ::= body-clause | binding-clause
;;;;   main-clause       ::= body-clause | selectable-clause
;;;;   body-clause       ::= initial-final | termination-test
;;;;   binding-clause    ::= with-clause  | for-as-clause
;;;;   selectable-clause ::= unconditional | accumulation | conditional
;;;;
;;;; termination-test is included in body-clause because Khazern allows termination test clauses
;;;; like UNTIL/WHILE to occur inbetween FOR/AS clauses. Khazern signals a STYLE-WARNING to to
;;;; indicate that this "might" be non-standard behavior.
;;;;
;;;; In 6.1.4 there is the statement "Termination-test control constructs can be used anywhere
;;;; within the loop body." This at best ambiguous in respect to the BNF grammar in the LOOP
;;;; dictionary entry and at worst a contradiction.
;;;;
;;;; SBCL permits UNTIL/WHILE clauses to be interspersed with variable clauses. It does not
;;;; allow termination tests aside from UNTIL/WHILE this freedom. This behavior was inherited
;;;; from CMUCL and this has been the norm since at least 1994. Casual inspection of projects in
;;;; Quicklisp by several individuals indicates that the assumption that this clause ordering is
;;;; permitted by the specification has become widespread.

;;; Mixin classes

(defclass var-mixin ()
  ((%var :accessor var
         :initarg :var)))

(defmethod map-variables (function (clause var-mixin))
  (map-variables function (var clause)))

(defclass compound-forms-mixin ()
  ((%forms :accessor forms
           :initarg :forms)))

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

;;; Clause classes

(defclass clause ()
  ((%start :accessor start
           :initarg :start
           :type fixnum)
   (%end :accessor end
         :initarg :end
         :type fixnum)))

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

(defclass binding-clause (clause)
  ())

(defmethod clause-group ((clause binding-clause))
  :variable)

(defmethod (setf clause-group) (group (clause binding-clause))
  (if (eq group :variable)
      group
      (error 'invalid-clause-order
             :clause (subseq *body* (start clause) (end clause))
             :found-group group
             :expected-group :variable)))

(defclass selectable-clause (clause)
  ())

(defmethod clause-group ((clause selectable-clause))
  :main)

(defmethod (setf clause-group) (group (clause selectable-clause))
  (if (eq group :main)
      group
      (error 'invalid-clause-order
             :clause (subseq *body* (start clause) (end clause))
             :found-group group
             :expected-group :main)))

(defclass body-clause (clause)
  ((%clause-group :accessor clause-group
                  :initform :main)))

(defclass selectable-superclause (selectable-clause)
  ())

