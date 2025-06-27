(cl:in-package #:khazern)

;;;; Clause classes and common methods
;;;;
;;;; The terminology used here is that of the BNF grammar in the dictionary description of the
;;;; loop macro in the HyperSpec.  It is not the same as the terminology used in the section
;;;; 6.1.
;;;;
;;;; In the dictionary entry for LOOP, the HyperSpec says:
;;;;
;;;; LOOP [name-clause] {variable-clause}* {main-clause}*
;;;;   variable-clause   ::= with-clause | initial-final | for-as-clause
;;;;   main-clause       ::= unconditional | accumulation | conditional | termination-test |
;;;;                         initial-final
;;;;   selectable-clause ::= unconditional | accumulation | conditional
;;;;
;;;; Since initial-final can be either a variable-clause or a main-clause we introduce new
;;;; clause groups called body-clause and binding-clause so the overall BNF of LOOP becomes:
;;;;
;;;; LOOP [name-clause] {variable-clause}* {main-clause}*
;;;;   variable-clause   ::= body-clause | binding-clause
;;;;   main-clause       ::= body-clause | selectable-clause
;;;;   body-clause       ::= initial-final | termination-test
;;;;   binding-clause    ::= with-clause  | for-as-clause
;;;;   selectable-clause ::= unconditional | accumulation | conditional
;;;;
;;;; termination-test is included in body-clause because Khazern allows termination test clauses
;;;; like UNTIL/WHILE to occur in between FOR/AS/WITH clauses. Khazern signals a STYLE-WARNING
;;;; to indicate that this "might" be non-standard behavior.
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

(defmethod map-variables progn (function (clause var-mixin))
  (map-variables function (var clause)))

(defmethod bindings nconc ((clause var-mixin))
  (d-spec-outer-bindings (var clause)))

(defmethod declarations nconc ((clause var-mixin))
  (d-spec-outer-declarations (var clause)))

(defclass other-var-mixin ()
  ((%other-var :accessor other-var
               :initarg :other-var)))

(defmethod map-variables progn (function (clause other-var-mixin))
  (map-variables function (other-var clause)))

(defmethod bindings nconc ((clause other-var-mixin))
  (d-spec-outer-bindings (other-var clause)))

(defmethod declarations nconc ((clause other-var-mixin))
  (d-spec-outer-declarations (other-var clause)))

(defclass compound-forms-mixin ()
  ((%forms :accessor forms
           :initarg :forms)))

(defclass form-mixin ()
  ((%form :accessor form
          :initarg :form)))

(defclass form-ref-mixin ()
  ((%form-ref :accessor form-ref
              :initarg :form-ref
              :initform nil
              :type symbol)))

(defclass accumulation-mixin ()
  ((%accum-var :accessor accum-var
               :initarg :accum-var
               :initform (make-instance 'simple-binding
                                        :var-spec (default-accumulation-variable)
                                        :accumulation-category nil))))

(defmethod initialize-instance :after ((instance accumulation-mixin) &rest initargs &key)
  (declare (ignore initargs))
  (unless (var-spec (accum-var instance))
    (setf (var-spec (accum-var instance)) (default-accumulation-variable))))


(defmethod map-variables progn (function (clause accumulation-mixin))
  (map-variables function (accum-var clause)))

;;; Clause classes

(defclass clause ()
  ((%start :accessor start
           :initarg :start
           :type fixnum)
   (%end :accessor end
         :initarg :end
         :type fixnum)
   (%simple-bindings :accessor simple-bindings
                     :initform nil)))

(defun add-simple-binding (clause
                           &key (var "FORM") (type t) accumulation-category (form nil formp)
                                ((:ignorable ignorablep) nil)
                                ((:dynamic-extent dynamic-extent-p) nil)
                                ((:fold foldp) nil) (fold-test 'constantp))
  (if (and foldp (funcall fold-test form))
      (values form nil)
      (let* ((ref (if (symbolp var)
                       var
                       (gensym var)))
             (binding (make-instance 'simple-binding
                                     :var-spec ref
                                     :type-spec type
                                     :accumulation-category accumulation-category
                                     :form (if formp
                                               form
                                               (deduce-initial-value type))
                                     :ignorable ignorablep
                                     :dynamic-extent dynamic-extent-p)))
        (setf (simple-bindings clause) (nconc (simple-bindings clause) (list binding)))
        (values ref binding))))

(defmethod bindings nconc ((clause clause))
  (mapcan (lambda (binding)
            (d-spec-simple-bindings binding (form binding)))
          (simple-bindings clause)))
  
(defmethod declarations nconc ((clause clause))
  (mapcan #'d-spec-simple-declarations (simple-bindings clause)))
  
(defclass simple-superclause (clause)
  ((%subclauses :accessor subclauses
                :initarg :subclauses
                :initform nil)))

(defmethod analyze :before ((clause simple-superclause))
  (mapc #'analyze (subclauses clause)))

(defmethod map-variables progn (function (clause simple-superclause))
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

(defmethod finish-step-forms ((clause sequential-superclause) initialp)
  (mapcan (lambda (clause)
            (nconc (begin-step-forms clause initialp)
                   (finish-step-forms clause initialp)))
          (subclauses clause)))

(defmethod wrap-forms ((clause sequential-superclause) forms)
  (wrap-let* (mapcan #'bindings (subclauses clause))
             (mapcan #'declarations (subclauses clause))
             (reduce #'wrap-forms (subclauses clause)
                     :from-end t :initial-value forms)))

(defclass parallel-superclause (simple-superclause)
  ())

(defmethod begin-step-forms ((clause parallel-superclause) initialp)
  (mapcan (lambda (clause)
            (begin-step-forms clause initialp))
          (subclauses clause)))

(defmethod finish-step-forms ((clause parallel-superclause) initialp)
  (mapcan (lambda (clause)
            (finish-step-forms clause initialp))
          (subclauses clause)))

(defmethod wrap-forms ((clause parallel-superclause) forms)
  (wrap-let (mapcan #'bindings (subclauses clause))
            (mapcan #'declarations (subclauses clause))
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

