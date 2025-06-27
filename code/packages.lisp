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
           #:add-simple-binding
           #:analyze
           #:begin-step-forms
           #:body-forms
           #:clause
           #:clause-group
           #:declarations
           #:define-interface
           #:destructuring-declarations
           #:destructuring-set
           #:destructuring-variable-list
           #:epilogue-forms
           #:expand-body
           #:finish-step-forms
           #:for-as-iteration-path
           #:iteration-path-preposition
           #:iteration-path-preposition-names
           #:iteration-path-using
           #:iteration-path-using-names
           #:make-destructuring-binding
           #:make-iteration-path
           #:map-variables
           #:prologue-forms
           #:standard-client
           #:subclauses
           #:symbol-equal
           #:var
           #:variable-list
           #:wrap-forms))
