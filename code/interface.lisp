(cl:in-package #:khazern)

(defclass standard-client ()
  ()
  (:documentation "The standard-client includes the core parsing and clause creation rules for
LOOP. KHAZERN:DEFINE-INTERFACE is then used with a subclass of the standard-client to create the
macro stubs LOOP and LOOP-FINISH."))

;;; Scopes

(defclass selectable-region () ())

(defclass body-region (selectable-region) ())

(defclass for-as-region () ())

(defclass being-region () ())

(defclass with-region () ())

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

(defun ensure-symbol (name &optional (package *package*))
  (intern (string name) package))

(defgeneric features-list (client)
  (:method-combination nconc)
  (:method nconc (client)
    (declare (ignore client))
    nil)
  (:method nconc ((client standard-client))
    (list :loop/khazern)))

(defmacro define-interface (client-var client-class &optional intrinsic)
  (declare (ignore client-class))
  "Create the macro stubs LOOP and LOOP-FINISH based on the client-var and client-class."
  (let* ((pkg (if intrinsic (find-package '#:common-lisp) *package*))
         (epilogue-tag (ensure-symbol '#:epilogue)))
    `(progn
       (defmacro ,(ensure-symbol '#:loop-finish pkg) ()
         '(go ,epilogue-tag))

       (defmacro ,(ensure-symbol '#:loop pkg) (&rest forms)
         (khazern:expand-body ,client-var forms ',epilogue-tag))

       ,@(when intrinsic
           `((eval-when (:compile-toplevel :load-toplevel :execute)
               (setf *features* (nunion (features-list ,client-var) *features*))))))))
