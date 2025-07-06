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
           #:d-type-spec 
           #:d-var-spec 
           #:declarations
           #:define-interface
           #:destructuring-set
           #:dynamic-extent-p
           #:epilogue-forms
           #:expand-body
           #:form
           #:ignorablep
           #:iteration-path-names
           #:make-iteration-path
           #:map-variables
           #:nullable-simple-var
           #:parse-clause
           #:parse-iteration-path-preposition
           #:parse-iteration-path-using
           #:pop-token
           #:pop-token?
           #:prologue-forms
           #:simple-var
           #:standard-client
           #:step-intro-forms
           #:step-outro-forms
           #:symbol-equal
           #:type-spec
           #:var-spec
           #:variable-list
           #:wrap-forms))
