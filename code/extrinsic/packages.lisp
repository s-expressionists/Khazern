(defpackage #:khazern-extrinsic
  (:use #:common-lisp)
  (:shadow #:loop
           #:loop-finish)
  (:export #:*parser-table*
           #:define-loop-path
           #:loop
           #:loop-finish))
