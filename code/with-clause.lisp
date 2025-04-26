(cl:in-package #:khazern)

;;; Clause WITH-CLAUSE.
;;;
;;; A WITH-CLAUSE allows the creation of local variables.  It is
;;; executed once.
;;;
;;; The syntax of a with-clause is:
;;;
;;;    with-clause ::= WITH var1 [type-spec] [= form1]
;;;                    {AND var2 [type-spec] [= form2]}*
;;;
;;; where var1 and var2 are destructuring variable specifiers
;;; (d-var-spec) allowing multiple local variables to be created in a
;;; single with-clause by destructuring the value of the corresponding
;;; form.
;;;
;;; When there are several consecutive with-clause, the execution is
;;; done sequentially, so that variables created in one with-clause
;;; can be used in the forms of subsequent with-clauses.  If parallel
;;; creation of variables is wanted, then the with-clause can be
;;; followed by one or more and-clauses.
;;;
;;; The (destructuring) type specifier is optional.  If no type
;;; specifier is given, it is as if t was given.
;;;
;;; The initialization form is optional.  If there is a corresponding
;;; type specifier for a variable, but no initialization form, then
;;; the variable is initialized to a value that is appropriate for the
;;; type.  In particular, for the type t the value is nil, for the
;;; type number, the value is 0, and for the type float, the value is
;;; 0.0.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class WITH-CLAUSE.
;;;

(defclass with-clause (variable-clause subclauses-mixin) ())

(defclass with-subclause (var-and-type-spec-mixin) ())

(defclass with-subclause-no-form (with-subclause) ())

(defclass with-subclause-with-form (with-subclause)
  ((%form :initarg :form :reader form)
   (%form-var :initform (gensym) :reader form-var)))

;;; The default form is NIL.
(defmethod form ((subclause with-subclause))
  nil)

(defmethod bound-variables ((clause with-clause))
  (mapcan #'bound-variables (subclauses clause)))

(defmethod bound-variables ((subclause with-subclause))
  (extract-variables (var-spec subclause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser with-subclause ()
  (consecutive (lambda (var-spec type-spec initargs)
                 (apply #'make-instance (if initargs
                                            'with-subclause-with-form
                                            'with-subclause-no-form)
                        :var-spec var-spec
                        :type-spec type-spec
                        initargs))
               'anything
               'optional-type-spec
               (optional nil
                         (consecutive (lambda (form)
                                        `(:form ,form))
                                      (keyword :=)
                                      'anything))))

(define-parser with-clause (:body-clause)
  (consecutive (lambda (first rest)
                 (make-instance 'with-clause
                                :subclauses (cons first rest)))
               (keyword :with)
               'terminal
               'with-subclause
               (repeat* #'cl:list
                        (consecutive #'identity
                                     (keyword :and)
                                     'terminal
                                     'with-subclause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the bindings.

(defmethod initial-bindings ((clause with-clause))
  (reduce #'append (mapcar #'initial-bindings (subclauses clause))))

(defmethod initial-bindings ((clause with-subclause-with-form))
  `((,(form-var clause) ,(form clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the subclause wrapper.

(defmethod wrap-subclause ((subclause with-subclause-with-form) inner-form)
  (wrap-let* (destructure-variables (var-spec subclause) (form-var subclause))
             '()
             inner-form))

(defmethod wrap-subclause ((subclause with-subclause-no-form) inner-form)
  (wrap-let (map-variable-types (lambda (var type)
                                  `(,var ,(if (subtypep type 'number)
                                              (coerce 0 type)
                                              nil)))
                                (var-spec subclause) (type-spec subclause))
            '()
            inner-form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the declarations.

(defmethod initial-declarations ((clause with-subclause))
  (reduce #'append (mapcar #'initial-declarations (subclauses clause))
          :from-end t))
