(cl:in-package #:khazern)

(defclass standard-client ()
  ()
  (:documentation "The standard-client includes the core parsing and clause creation rules for
LOOP. KHAZERN:DEFINE-INTERFACE is then used with a subclass of the standard-client to create the
macro stubs LOOP and LOOP-FINISH."))

;;; Scopes

(defclass selectable-region ()
  ()
  (:documentation "The clause region for subclauses of conditionals"))

(defclass body-region (selectable-region)
  ()
  (:documentation "The top-level clause region which includes selectable clauses."))

(defclass for-as-region ()
  ()
  (:documentation "The clause region for subclauses of FOR and AS."))

(defclass being-region ()
  ()
  (:documentation "The clause region for subclauses of FOR-AS-BEING."))

(defclass with-region ()
  ()
  (:documentation "The clause region for subclauses of WITH."))

;;; Variable mapping

(defgeneric map-variables (function instance)
  (:documentation "Map over the variables associated with the instance. For each variable call
the FUNCTION with the arguments var, type and category.")
  (:method-combination progn)
  (:method progn (function instance)
    (declare (ignore function instance))
    nil)
  (:method progn (function (instance list))
    (mapc (lambda (item)
            (map-variables function item))
          instance)
    nil))

;;; Expansion interface

(defgeneric assignment-pairs (binding form)
  (:documentation "THis generic function returns a list of locations and forms to be used in SETQ.")
  (:method (binding form)
    (declare (ignore binding form))
    nil)
  (:method ((binding symbol) form)
      (when binding
        (list binding form))))

(defgeneric variable-list (clause)
  (:documentation "This generic function returns a list of variable and initial forms for use in
LET or LET*.")
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric declarations (clause)
  (:documentation "The purpose of this generic function is to extract a list of declaration
specifiers from the clause.  Notice that it is a list of declaration specifiers, not a list of
declarations.  In other words, the symbol DECLARE is omitted.")
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric prologue-forms (clause)
  (:documentation "This generic function returns forms for CLAUSE that should go in the LOOP
prologue. The INITIALLY clause is an obvious candidate for such code.")
  (:method (clause)
    (declare (ignore clause))
    nil))

(defgeneric step-intro-forms (clause initialp)
  (:documentation "This generic functions returns the forms that prepare to step a clause. This
typically includes advancing the internal iterator variables and checking for termination
conditions. INITIALP is non-NIL for the step forms completed before the body forms.")
  (:method (clause initialp)
    (declare (ignore clause initialp))
    nil))

(defgeneric step-outro-forms (clause initialp)
  (:documentation "This generic functions returns the forms that complete stepping a clause.
This typically includes setting destructuring loop variables and USING variables. INITIALP is
non-NIL for the step forms completed before the body forms.")
  (:method (clause initialp)
    (declare (ignore clause initialp))
    nil))

(defgeneric body-forms (clause)
  (:documentation "This generic function returns forms for CLAUSE that should go in body, after
the introduction step forms.")
  (:method (clause)
    (declare (ignore clause))
    nil))

(defgeneric epilogue-forms (clause)
  (:documentation "This generic function returns forms for CLAUSE that should go in the LOOP
epilogue. The FINALLY clause is an obvious candidate for such code.")
  (:method (clause)
    (declare (ignore clause))
    nil))

(defgeneric afterword-forms (clause)
  (:documentation "THis generic function returns cleanup forms for CLAUSE that should be placed in UNWIND-PROTECT.")
  (:method (clause)
    (declare (ignore clause))
    nil))

(defgeneric wrap-forms (clause forms)
  (:documentation "Once the LOOP prologue, the LOOP body, and the LOOP epilogue have all been
constructed, a bunch of successive WRAPPERS are applied so as to obtain the final expansion.
Each clause type defines how it needs to be wrapped.  Some clauses only require the
establishment of variable bindings in the wrapper.  Other clauses might need to be wrapped in
some iterator form.  The generic function WRAP-CLAUSE defines how each clause type is wrapped.")
  (:method (clause forms)
    (declare (ignore clause))
    forms))

(defgeneric name (clause)
  (:documentation "This generic function returns the block name of the LOOP.")
  (:method (clause)
    (declare (ignore clause))
    nil))

(defgeneric clause-group (clause)
  (:documentation "Return the top level group of the clause. This is either :NAME, :VARIABLE or
:MAIN"))

(defgeneric (setf clause-group) (group clause)
  (:documentation "Some clauses can appear in either the :VARIABLE or :MAIN group. This SETF
provides a way to inform the clause where it located so that it knows whether to put its
expansion into the step forms or the body forms. If the clause cannot be moved it should signal
INVALID-CLAUSE-ORDER or warn POSSIBLE-INVALID-CLAUSE-ORDER."))

;;; Binding interface

(defgeneric var-spec (binding)
  (:documentation "Return the var-spec of the binding. This is possibly a d-var-spec."))

(defgeneric (setf var-spec) (value binding)
  (:documentation "Set the var-spec of the binding."))

(defgeneric type-spec (binding)
  (:documentation "Return the type-spec of the binding. This is possibly a d-type-spec."))

(defgeneric (setf type-spec) (value binding)
  (:documentation "Set the type-spec of the binding."))

(defgeneric ignorablep (binding)
  (:documentation "Return non-NIL if the binding is declared IGNORABLE."))

(defgeneric (setf ignorablep) (value binding)
  (:documentation "Set the IGNORABLE declaration of the binding. Non-NIL if the binding is
declared IGNORABLE."))

(defgeneric dynamic-extent-p (binding)
  (:documentation "Return non-NIL if the binding is declared DYNAMIC-EXTENT."))

(defgeneric (setf dynamic-extent-p) (value binding)
  (:documentation "Set the DYNAMIC-EXTENT declaration of the binding. Non-NIL if the binding is
declared DYNAMIC-EXTENT."))

(defgeneric category (binding)
  (:documentation "Return the accumulation category of the binding. There can be multiple
accumulation clauses that have the same binding name as long as they all have the same
accumulation category. The category should be a keyword. Currently used categories are :LIST,
:SUMMATION, :EXTREMUM, :EVERY and :SOME.")
  (:method (binding)
    (declare (ignore binding))))

(defgeneric (setf category) (value binding)
  (:documentation "Set the accumulation category of the binding."))

(defgeneric scope-references (binding)
  (:documentation "Return the accumulation category of the binding. There can be multiple
accumulation clauses that have the same binding name as long as they all have the same
accumulation category. The category should be a keyword. Currently used categories are :LIST,
:SUMMATION, :EXTREMUM, :EVERY and :SOME.")
  (:method (binding)
    (declare (ignore binding))
    nil))

(defgeneric (setf scope-references) (value binding)
  (:documentation "Set the accumulation category of the binding."))

;;; Accumulation clause interface

(defgeneric make-scope (client name type category references)
  (:documentation "Create an accumulation clause based on the category. This is the clause that
will contain the bindings or wrappers that are common to all of the accumulation clauses and
will be inserted at the beginning of the clause list."))

(defgeneric scope-reference (instance ref)
  (:documentation "Return a symbol that represents the lexical name of REF in the accumulation
variable NAME. For example, a REF of :TAIL for list accumulation will return the tail cons of
the accumulation variable.")
  (:method (instance ref)
    (declare (ignore instance ref))
    nil))

(defgeneric scope-functions (client instance reference name)
  (:documentation "Return a list of function definitions suitable for LABELS.")
  (:method (client instance reference name)
    (declare (ignore client instance reference name))
    nil))

;;; Parsing interface

(defgeneric parse-clause (client region name &key)
  (:documentation "Parse a clause based on its keyword name."))

(defgeneric preposition-names (client instance)
  (:documentation "Return (VALUES PREPOSITION-NAMES REQUIRED-PREPOSITION-NAMES USING-NAMES).
Each is a list of names or name groups.")
  (:method (client instance)
    (declare (ignore client instance))
    (values nil nil nil)))

(defgeneric parse-preposition (client instance name)
  (:documentation "Parse the next item as a preposition value."))

(defgeneric parse-using (client instance name)
  (:documentation "Parse the next item as a using value."))

;;; Clause analysis

(defgeneric analyze (client clause-or-clauses)
  (:documentation "Analyze the clause for semantic errors.")
  (:method (client clause-or-clauses)
    (declare (ignore client clause-or-clauses))))

;;; Interface declaration

(defmethod trinsic:features-list nconc ((client standard-client))
  (list :loop/khazern))

(trinsic:make-define-interface (:client-form client-form)
    ((loop-sym cl:loop)
     (loop-finish-sym cl:loop-finish))
  (let ((epilogue-tag (unique-name :epilogue)))
    `((defmacro ,loop-finish-sym ()
        '(go ,epilogue-tag))

      (defmacro ,loop-sym (&rest forms)
        (khazern:expand-body ,client-form forms ',epilogue-tag)))))
