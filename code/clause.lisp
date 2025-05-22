(cl:in-package #:khazern)

;;;; The terminology used here is that of the BNF grammar in the
;;;; dictionary description of the loop macro in the HyperSpec.  It is
;;;; not the same as the terminology used in the section 6.1.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Common classes.

;;; The base class of all clauses.
(defclass clause ()
  ())

;;; Mixin for clauses that accept `AND'.
(defclass subclauses-mixin ()
  ((%subclauses :initarg :subclauses :accessor subclauses)))

(defclass selectable-clauses () ())

(defmethod normalize-token (client (scope selectable-clauses) (token symbol))
  (symbol-lookup token
                 '((:and . :and)
                   (:append . :append)
                   (:appending . :append)
                   (:collect . :collect)
                   (:collecting . :collect)
                   (:count . :count)
                   (:counting . :count)
                   (:do . :do)
                   (:doing . :doing)
                   (:else . :else)
                   (:end . :end)
                   (:if . :if)
                   (:into . :into)
                   (:of-type :of-type)
                   (:maximize . :maximize)
                   (:maximizing . :maximize)
                   (:minimize . :minimize)
                   (:minimizing . :minimize)
                   (:nconc . :nconc)
                   (:nconcing . :nconc)
                   (:return . :return)
                   (:sum . :sum)
                   (:summing . :sum)
                   (:unless . :unless)
                   (:when . :if))
                 token))

(defclass body-clauses (selectable-clauses) ())

(defmethod normalize-token (client (scope body-clauses) (token symbol))
  (symbol-lookup token
                 '((:and . :and)
                   (:always . :always)
                   (:append . :append)
                   (:appending . :append)
                   (:as . :for)
                   (:collect . :collect)
                   (:collecting . :collect)
                   (:count . :count)
                   (:counting . :count)
                   (:do . :do)
                   (:doing . :do)
                   (:else . :else)
                   (:end . :end)
                   (:finally . :finally)
                   (:for . :for)
                   (:if . :if)
                   (:initially . :initially)
                   (:into . :into)
                   (:maximize . :maximize)
                   (:maximizing . :maximize)
                   (:minimize . :minimize)
                   (:minimizing . :minimize)
                   (:named . :named)
                   (:nconc . :nconc)
                   (:nconcing . :nconc)
                   (:never . :never)
                   (:of-type . :of-type)
                   (:repeat . :repeat)
                   (:return . :return)
                   (:sum . :sum)
                   (:summing . :sum)
                   (:thereis . :thereis)
                   (:unless . :unless)
                   (:until . :until)
                   (:when . :if)
                   (:while . :while)
                   (:with . :with))
                 token))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Expansion methods for FOR-AS clause.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the bindings.

(defmethod initial-bindings ((clause subclauses-mixin))
  (mapcan #'initial-bindings (subclauses clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the declarations.

(defmethod initial-declarations ((clause subclauses-mixin))
  (mapcan #'initial-declarations (subclauses clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue-forms.

(defmethod prologue-forms ((clause subclauses-mixin))
  (mapcan #'prologue-forms (subclauses clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step forms.

(defmethod initial-step-forms ((clause subclauses-mixin))
  (wrap-let* (mapcan #'initial-step-bindings (subclauses clause))
             (mapcan #'initial-step-declarations (subclauses clause))
             (mapcan #'initial-step-forms (subclauses clause))))

(defmethod subsequent-step-forms ((clause subclauses-mixin))
  (wrap-let* (mapcan #'subsequent-step-bindings (subclauses clause))
             (mapcan #'subsequent-step-declarations (subclauses clause))
             (mapcan #'subsequent-step-forms (subclauses clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the body-forms.

(defmethod body-forms ((clause subclauses-mixin))
  (mapcan #'body-forms (subclauses clause)))

(defmethod epilogue-forms ((clause subclauses-mixin))
  (mapcan #'epilogue-forms (subclauses clause)))

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
;;; Mixin for clauses that make the loop return a value.

(defclass loop-return-clause-mixin ()
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin for clauses that has an implicit IT argument.

(defclass it-mixin ()
  ())

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

(defmethod main-clause-p ((clause main-clause))
  t)

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

(defmethod variable-clause-p ((clause variable-clause))
  t)

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
