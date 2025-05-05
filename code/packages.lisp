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
  (:export #:alternative
           #:anything
           #:body-forms
           #:consecutive
           #:copy-cons-cells
           #:copy-parser-table
           #:define-parser
           #:epilogue-forms
           #:expand-body
           #:final-bindings
           #:final-declarations
           #:generate-assignments
           #:generate-variable-bindings
           #:generate-variable-declarations
           #:initial-bindings
           #:initial-declarations
           #:keyword
           #:list
           #:main-clause-p
           #:map-variable-types
           #:map-variables
           #:%map-variables
           #:name-clause-p
           #:optional
           #:optional-type-spec
           #:parser-enabled-p
           #:prologue-bindings
           #:prologue-forms
           #:repeat*
           #:repeat+
           #:singleton
           #:step-bindings
           #:step-forms
           #:subclauses
           #:terminal
           #:termination-forms
           #:typep
           #:variable-clause-p))
