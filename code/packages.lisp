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
           #:add-simple-binding
           #:analyze
           #:step-intro-forms
           #:body-forms
           #:check-nullable-simple-var-spec
           #:clause
           #:clause-group
           #:declarations
           #:define-interface
           #:destructuring-declarations
           #:destructuring-set
           #:destructuring-variable-list
           #:dynamic-extent-p
           #:epilogue-forms
           #:expand-body
           #:step-outro-forms
           #:for-as-iteration-path
           #:form
           #:ignorablep
           #:iteration-path-preposition
           #:iteration-path-preposition-names
           #:iteration-path-using
           #:iteration-path-using-names
           #:make-destructuring-binding
           #:make-iteration-path
           #:map-variables
           #:prologue-forms
           #:standard-client
           #:symbol-equal
           #:type-spec
           #:var
           #:var-spec
           #:variable-list
           #:wrap-forms))
