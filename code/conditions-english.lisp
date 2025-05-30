(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition reporters for parse errors.

(defmethod acclimation:report-condition
    ((condition unknown-parser) stream (language acclimation:english))
  (format stream
          "Unknown parser ~a with client ~a and scope ~a."
          (name condition) (class-name (class-of (client condition)))
          (class-name (class-of (scope condition)))))

(defmethod acclimation:report-condition
    ((condition unknown-iteration-path) stream (language acclimation:english))
  (format stream
          "Unknown ~:[~;inclusive ~]iteration path ~a with client ~a and scope ~a."
          (inclusivep condition) (name condition)
          (class-name (class-of (client condition)))
          (class-name (class-of (scope condition)))))

(defmethod acclimation:report-condition
    ((condition missing-iteration-path-prepositions) stream (language acclimation:english))
  (format stream
          "Missing ~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~} preposition~p for ~:[~;inclusive ~]iteration path ~a."
          (names condition) (length (names condition))
          (inclusivep condition) (name condition)))

(defmethod acclimation:report-condition
    ((condition invalid-iteration-path-preposition-order) stream (language acclimation:english))
  (format stream
          "Preposition order of ~a followed by ~a is invalid for ~:[~;inclusive ~]iteration path ~a."
          (first-preposition condition) (second-preposition condition)
          (inclusivep condition) (name condition)))

(defmethod acclimation:report-condition
    ((condition expected-token-but-end) stream
     (language acclimation:english))
  (format stream
          "Expected~:[~; either~]~
           ~@[ a loop keyword of ~{~#[~;~a~;~a or ~a~:;~@{~a~#[~;, or ~:;, ~]~}~]~}~]~
           ~:[~; or~]~@[ a token of type ~s~]~
           ~@[ at location ~s~] but reached the end of the loop body instead."
          (and (expected-keywords condition)
               (expected-type condition))
          (expected-keywords condition)
          (and (expected-keywords condition)
               (expected-type condition))
          (expected-type condition)
          (location condition)))

(defmethod acclimation:report-condition
    ((condition expected-token-but-found)
     stream
     (language acclimation:english))
  (format stream
          "Expected~:[~; either~]~
           ~@[ a loop keyword of ~{~#[~;~a~;~a or ~a~:;~@{~a~#[~;, or ~:;, ~]~}~]~}~]~
           ~:[~; or~]~@[ a token of type ~s~]~
           ~@[ at location ~s~] but found ~a instead."
          (and (expected-keywords condition)
               (expected-type condition))
          (expected-keywords condition)
          (and (expected-keywords condition)
               (expected-type condition))
          (expected-type condition)
          (location condition)
          (found condition)))

(defmethod acclimation:report-condition
    ((condition unexpected-token-found)
     stream
     (language acclimation:english))
  (format stream
          "Unexpected token ~s found~@[at location ~s ~]."
          (found  condition)
          (location condition)))

(defmethod acclimation:report-condition
    ((condition expected-var-spec-but-end)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
  (format stream
          "Expected a variable specification, but reached~@
           the end of the loop body."))

(defmethod acclimation:report-condition
    ((condition expected-var-spec-but-found)
     stream
     (language acclimation:english))
  (format stream
          "Expected a variable specification but found~@
           the following instead:~@
           ~s"
          (found condition)))

(defmethod acclimation:report-condition
    ((condition too-many-prepositions-from-one-group)
     stream
     (language acclimation:english))
  (format stream
          "Expected (hash-value other-var), but found~@
           the following instead:~@
           ~s"
          (found condition)))

(defmethod acclimation:report-condition
    ((condition conflicting-stepping-directions)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
  (format stream
          "Conflicting stepping directions."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition reporters for syntax errors.

(defmethod acclimation:report-condition
    ((condition non-compound-form)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
  (format stream
          "A non-compound form was found in a simple~@
           loop."))

(defmethod acclimation:report-condition
    ((condition name-clause-not-first)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
  (format stream
          "A NAME loop clause was found, but it was~@
           not the first clause."))

(defmethod acclimation:report-condition
    ((condition multiple-name-clauses)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
  (format stream
          "Multiple NAME clauses where found."))

(defmethod acclimation:report-condition
    ((condition invalid-clause-order)
     stream
     (language acclimation:english))
  (declare (ignorable condition))
  (format stream
          "Invalid clause order.  Variable clauses must precede main clauses."))

(defmethod acclimation:report-condition
    ((condition multiple-variable-occurrences)
     stream
     (language acclimation:english))
  (format stream
          "Multiple occurrences of the following variable were found:~@
           ~s"
          (bound-variable condition)))

(defmethod acclimation:report-condition
    ((condition iteration-accumulation-overlap)
     stream
     (language acclimation:english))
  (format stream
          "The variable ~s is used both as an iteration variable~@
          and as an accumulation variable."
          (bound-variable condition)))

(defmethod acclimation:report-condition
    ((condition multiple-accumulation-occurrences)
     stream
     (language acclimation:english))
  (format stream
          "the accumulation variable~@[ ~s~] is used both~@
           for ~a accumulation and ~a accumulation."
          (bound-variable condition)
          (first-clause condition)
          (second-clause condition)))

(defmethod acclimation:report-condition
    ((condition loop-path-non-inclusive)
     stream
     (language acclimation:english))
  (format stream
          "Inclusive iteration is not possible with the ~a LOOP iteration path."
          (path condition)))

(defmethod acclimation:report-condition
    ((condition unknown-data-type)
     stream
     (language acclimation:english))
  (format stream
          "Cannot verify that ~s is a subtype of the required type ~s."
          (subtype condition) (supertype condition)))

(defmethod acclimation:report-condition
    ((condition invalid-data-type)
     stream
     (language acclimation:english))
  (format stream
          "Specified type ~s is not a subtype of ~s."
          (subtype condition) (supertype condition)))

(defmethod acclimation:report-condition
    ((condition non-nullable-simple-d-var-spec)
     stream
     (language acclimation:english))
  (format stream
          "Specified d-var-spec ~s is not a simple-var or NIL."
          (var-spec condition)))
