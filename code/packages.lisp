(cl:in-package #:common-lisp-user)

(defpackage #:khazern
  (:use #:common-lisp)
  (:shadow
   #:condition)
  (:export #:*accumulation-var*
           #:*epilogue-tag*
           #:*it-var*
           #:*loop-name*
           #:accumulation-category
           #:analyze
           #:body-forms
           #:clause-group
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
           #:initial-bindings
           #:initial-declarations
           #:initial-step-bindings
           #:initial-step-declarations
           #:initial-step-forms
           #:iteration-path-preposition
           #:iteration-path-preposition-names
           #:iteration-path-using
           #:iteration-path-using-names
           #:make-iteration-path
           #:map-variables
           #:prologue-forms
           #:standard-client
           #:subclauses
           #:subsequent-step-bindings
           #:subsequent-step-declarations
           #:subsequent-step-forms
           #:symbol-equal
           #:var
           #:wrap-forms))
