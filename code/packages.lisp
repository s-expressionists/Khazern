(cl:in-package #:common-lisp-user)

(defpackage #:khazern
  (:use #:common-lisp)
  (:shadow
   ;; We use TYPE as an accessor for a TYPE-SPEC so we need to shadow
   ;; this name.
   #:type
   ;; We use CONDITION as an accessor for a conditional clause so we
   ;; need to shadow this name
   #:condition
   #:loop
   #:loop-finish)
  (:export #:clause
           #:compound-forms-mixin
           #:define-parser
           #:expand-body
           #:loop
           #:loop-finish
           #:loop-return-clause-mixin
           #:subclauses-mixin
           #:var-and-type-spec-mixin))
