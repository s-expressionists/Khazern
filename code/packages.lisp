(cl:in-package #:common-lisp-user)

(defpackage #:khazern
  (:use #:common-lisp)
  (:shadow
   #:keyword
   #:list
   ;; We use TYPE as an accessor for a TYPE-SPEC so we need to shadow
   ;; this name.
   #:type
   ;; We use CONDITION as an accessor for a conditional clause so we
   ;; need to shadow this name
   #:typep
   #:condition)
  (:export #:accumulation-variables
           #:alternative
           #:anything
           #:body-form
           #:bound-variables
           #:consecutive
           #:copy-cons-cells
           #:copy-parser-table
           #:define-parser
           #:destructure-variables
           #:epilogue-form
           #:expand-body
           #:extract-variables
           #:final-bindings
           #:final-declarations
           #:fresh-variables
           #:generate-assignments
           #:generate-variable-bindings
           #:generate-variable-declarations
           #:initial-bindings
           #:initial-declarations
           #:keyword
           #:list
           #:main-clause-p
           #:map-variable-types
           #:name-clause-p
           #:optional
           #:optional-type-spec
           #:parser-enabled-p
           #:prologue-form
           #:prologue-form-bindings
           #:repeat*
           #:repeat+
           #:singleton
           #:step-form
           #:step-form-bindings
           #:subclauses
           #:typep
           #:termination-form
           #:terminal
           #:variable-clause-p))
