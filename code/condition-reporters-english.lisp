(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition reporters for parse errors.

(defmethod acclimation:report-condition
    ((condition expected-token-but-end)
     stream
     (language acclimation:english))
  (format stream
          "Expected a token~:[~; either~]~
           ~@[ being a loop keyword of ~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, or ~:;, ~]~}~]~}~]~
           ~:[~; or~]~@[ being of type ~s~]~
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
          "Expected a token~:[~; either~]~
           ~@[ being a loop keyword of ~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, or ~:;, ~]~}~]~}~]~
           ~:[~; or~]~@[ being of type ~s~]~
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
    ((condition unexpected-tokens-found)
     stream
     (language acclimation:english))
  (format stream
          "Unexpected tokens ~s found~@[at location ~s ~]."
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
