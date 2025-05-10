(cl:in-package #:khazern)

(defgeneric map-variables (function clause)
  (:method (function clause)
    (declare (ignore function clause))
    nil)
  (:method (function (clause cl:list))
    (mapc (lambda (subclause)
            (map-variables function subclause))
          clause)
    nil))

;;; The purpose of this generic function is to extract a list of
;;; declaration specifiers from the clause.  Notice that it is a list
;;; of declaration specifiers, not a list of declarations.  In other
;;; words, the symbol DECLARE is omitted.
(defgeneric initial-declarations (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric final-declarations (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric initial-bindings (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric final-bindings (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

;;; This generic function returns a binding for CLAUSE that should go in
;;; the LOOP prologue.
(defgeneric prologue-bindings (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

;;; This generic function returns a form for CLAUSE that should go in
;;; the LOOP prologue.  The INITIALLY clause is an obvious candidate
;;; for such code.  But the stepping clauses also have code that goes
;;; in the prologue, namely an initial termination test to determine
;;; whether any iterations at all should be executed.  END-TAG is the
;;; tag to GO to when the initial termination test says that no
;;; iterations should be executed.
(defgeneric prologue-forms(clause end-tag)
  (:method (clause end-tag)
    (declare (ignore clause end-tag))
    nil))

;;; This generic function returns a form for CLAUSE that should go
;;; between the body code and the stepping forms in the body of the
;;; expanded code.  Some of the FOR-AS clauses and also the REPEAT
;;; clause generate code here.  END-TAG is the tag to GO to when
;;; iteration should terminate.
(defgeneric termination-forms (clause end-tag)
  (:method (clause end-tag)
    (declare (ignore clause end-tag))
    nil))

;;; This generic function returns a form for CLAUSE that should go in
;;; the main the body code, before the termination test and the
;;; stepping forms, in the body of the expanded code.  The DO clause
;;; and the accumulation clauses are obvious candidates for such code.
;;;
;;; FIXME: Currently, END-TAG is used only in the WHILE clause as a
;;; termination test.  Investigate whether the WHILE clause should use
;;; TERMINATION-TEST instead, so that we can eliminate this parameter.
(defgeneric body-forms (clause end-tag)
  (:method (clause end-tag)
    (declare (ignore clause end-tag))
    nil))

;;; This generic function returns the bindings for CLAUSE that go
;;; after the main body code and the termination tests in the body of
;;; the expanded code.
(defgeneric step-bindings (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

;;; This generic function returns a form for CLAUSE that should go
;;; after the main body code and the termination tests in the body of
;;; the expanded code.  The FOR-AS clauses and also the REPEAT clause
;;; generate code here.
(defgeneric step-forms (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

;;; This generic function returns a form for CLAUSE that should go in
;;; the LOOP epilogue.  Of the clause types defined by the Common Lisp
;;; standard, only the method specialized to the FINALLY clause
;;; returns a value other than NIL.
(defgeneric epilogue-forms (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

;;; Once the LOOP prologue, the LOOP body, and the LOOP epilogue have
;;; all been constructed, a bunch of successive WRAPPERS are applied
;;; so as to obtain the final expansion.  Each clause type defines how
;;; it needs to be wrapped.  Some clauses only require the
;;; establishment of variable bindings in the wrapper.  Other clauses
;;; might need to be wrapped in some iterator form.  The generic
;;; function WRAP-CLAUSE defines how each clause type is wrapped.
(defgeneric wrap-clause (clause inner-form))

;;; If a clause can have subclauses, then each subclause may need to
;;; be wrapped separately.  The generic function WRAP-SUBCLAUSE
;;; determines how this is done.
(defgeneric wrap-subclause (subclause inner-form))

(defgeneric main-clause-p (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

(defgeneric name-clause-p (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

(defgeneric variable-clause-p (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

;;; We define three different accumulation CATEGORIES, each identified
;;; by a symbol: LIST, COUNT/SUM, and MAX/MIN.  Accumulation clauses
;;; within a category are compatible in that they can be mixed, even
;;; when they accumulate into the same variable.  This generic
;;; function takes an accumulation clause and returns the category.
(defgeneric accumulation-category (clause))

(defgeneric path-inclusive-permitted-p (instance)
  (:method (instance)
    (declare (ignore instance))
    nil))

(defgeneric path-inclusive-p (instance))

(defgeneric (setf path-inclusive-p) (instance))

(defgeneric path-preposition-key (instance name))

(defgeneric path-preposition (instance key))

(defgeneric (setf path-preposition) (new-value instance name))

(defgeneric subclauses (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric analyze (clause-or-clauses)
  (:method (clause-or-clauses)
    (declare (ignore clause-or-clauses)))
  (:method :before (clause-or-clauses)
    (mapc #'analyze (subclauses clause-or-clauses))))

;;; Parser Table interface

(defgeneric copy-parser-table (table))

(defgeneric parser-enabled-p (table name))

(defgeneric (setf parser-enabled-p) (value table name))

(defgeneric add-path (table constructor &rest names))
