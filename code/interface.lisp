(cl:in-package #:khazern)

(defclass standard-client ()
  ())

;;; Parsing interface

(defgeneric parse-clause (client scope name)
  (:documentation "Parse a clause based on its keyword name."))

;;; Variable mapping

(defgeneric map-variables (function clause)
  (:method-combination progn)
  (:method progn (function clause)
    (declare (ignore function clause))
    nil)
  (:method progn (function (clause list))
    (mapc (lambda (subclause)
            (map-variables function subclause))
          clause)
    nil))

;;; Expansion interface

(defgeneric variable-list (clause)
  (:documentation "This generic function returns a list of variable and initial forms for use in
LET or LET*.")
  (:method-combination nconc)
  (:method nconc (clause)
    (declare (ignore clause))
    '()))

(defgeneric declarations (clause)
  (:documentation "The purpose of this generic function is to extract a list of declaration
specifiers from the clause.  Notice that it is a list of declaration specifiers, not a list of
declarations.  In other words, the symbol DECLARE is omitted.")
  (:method-combination nconc)
  (:method nconc (clause)
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

(defgeneric var-spec (binding))

(defgeneric (setf var-spec) (value binding))

(defgeneric type-spec (binding))

(defgeneric (setf type-spec) (value binding))

(defgeneric ignorablep (binding))

(defgeneric (setf ignorablep) (value binding))

(defgeneric dynamic-extent-p (binding))

(defgeneric (setf dynamic-extent-p) (value binding))

(defgeneric accumulation-category (binding)
  (:documentation "Return the accumulation category of the binding.")
  (:method (binding)
    (declare (ignore binding))))

(defgeneric (setf accumulation-category) (value binding))

;;; Accumulation clause interface

(defgeneric make-accumulation-clause (name type category))

(defgeneric accumulation-clause-reference (instance name ref)
  (:method (instance name ref)
    (declare (ignore instance name ref))
    nil))

;;; Iteration path interface

(defgeneric make-iteration-path (client name &optional inclusive-form))

(defgeneric iteration-path-preposition-names (instance)
  (:method (instance)
    (declare (ignore instance))
    nil))

(defgeneric iteration-path-required-preposition-names (instance)
  (:method (instance)
    (declare (ignore instance))
    nil))

(defgeneric iteration-path-preposition (instance key))

(defgeneric (setf iteration-path-preposition) (new-value instance name))

(defgeneric iteration-path-using-names (instance)
  (:method (instance)
    (declare (ignore instance))
    nil))

(defgeneric iteration-path-using (instance key))

(defgeneric (setf iteration-path-using) (new-value instance name))

;;; Clause analysis

(defgeneric analyze (clause-or-clauses)
  (:method (clause-or-clauses)
    (declare (ignore clause-or-clauses))))

;;; Interface declaration

(defun ensure-symbol (name &optional (package *package*))
  (intern (string name) package))

(defmacro define-interface (client-var client-class &optional intrinsic)
  (declare (ignore client-class))
  (let* ((intrinsic-pkg (if intrinsic (find-package '#:common-lisp) *package*))
         (epilogue-tag (ensure-symbol '#:epilogue)))
    `(progn
       (defmacro ,(ensure-symbol '#:loop-finish intrinsic-pkg) ()
         '(go ,epilogue-tag))

       (defmacro ,(ensure-symbol '#:loop intrinsic-pkg) (&rest forms)
         (khazern:expand-body ,client-var forms ',epilogue-tag)))))
