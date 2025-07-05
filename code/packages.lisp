(cl:in-package #:common-lisp-user)

(defpackage #:khazern
  (:use #:common-lisp)
  (:shadow
   #:condition)
  (:export #:*accumulation-var*
           #:*epilogue-tag*
           #:*it-var*
           #:*loop-name*
           #:*placeholder-result*
           #:accumulation-category
           #:add-binding
           #:add-destructuring-binding
           #:add-simple-binding
           #:analyze
           #:body-forms
           #:check-nullable-simple-var-spec
           #:clause
           #:clause-group
           #:declarations
           #:define-interface
           #:destructuring-set
           #:dynamic-extent-p
           #:epilogue-forms
           #:expand-body
           #:for-as-iteration-path
           #:form
           #:ignorablep
           #:iteration-path-preposition
           #:iteration-path-preposition-names
           #:iteration-path-using
           #:iteration-path-using-names
           #:make-iteration-path
           #:map-variables
           #:prologue-forms
           #:standard-client
           #:step-intro-forms
           #:step-outro-forms
           #:symbol-equal
           #:type-spec
           #:var
           #:var-spec
           #:variable-list
           #:wrap-forms))
