(cl:in-package #:common-lisp-user)

(defpackage #:khazern-intrinsic
  (:use #:common-lisp)
  (:export #:*parser-table*
           #:get-loop-path
           #:remove-loop-path
           #:set-loop-path))
 
