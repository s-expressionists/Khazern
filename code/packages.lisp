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
  (:export #:*accumulation-var*
           #:*epilogue-tag*
           #:*it-var*
           #:*loop-name*
           #:accumulation-category
           #:analyze
           #:body-forms
           #:copy-cons-cells
           #:d-spec
           #:d-spec-inner-assignments
           #:d-spec-inner-bindings
           #:d-spec-inner-form
           #:d-spec-outer-bindings
           #:d-spec-outer-declarations
           #:define-interface
           #:epilogue-forms
           #:expand-body
           #:final-bindings
           #:final-declarations
           #:initial-bindings
           #:initial-declarations
           #:initial-step-bindings
           #:initial-step-declarations
           #:initial-step-forms
           #:main-clause-p
           #:map-variables
           #:name-clause-p
           #:path-preposition
           #:path-preposition-names
           #:path-using
           #:path-using-names
           #:prologue-forms
           #:subclauses
           #:subsequent-step-bindings
           #:subsequent-step-declarations
           #:subsequent-step-forms
           #:symbol-equal
           #:symbol-lookup
           #:var
           #:variable-clause-p
           #:wrap-clause
           #:wrap-subclause))
