(defpackage #:khazern-extrinsic
  (:use #:common-lisp)
  (:shadow #:loop
           #:loop-finish)
  (:export #:*parser-table*
           #:loop
           #:loop-finish))
