(cl:in-package #:khazern)

;;; We define a class that is the root class of all termination-test
;;; clauses.  Recall that a termination-test clause is a main clause,
;;; and that the HyperSpec defines TERMINATION-TEST as follows:
;;;
;;;   termination-test ::= while form |
;;;                        until form |
;;;                        repeat form |
;;;                        always form |
;;;                        never form |
;;;                        thereis form

(defclass termination-test-clause (main-clause)
  ())

(defclass boolean-termination-test-clause (termination-test-clause form-mixin)
  ())

(defmethod map-variables (function (clause boolean-termination-test-clause))
  (funcall function
           (default-accumulation-variable) t
           (accumulation-category clause)))
