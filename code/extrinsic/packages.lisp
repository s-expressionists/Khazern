(defpackage #:khazern-extrinsic
  (:use #:common-lisp)
  (:shadow #:loop
           #:loop-finish)
  (:export #:*client*
           #:extrinsic-client
           #:loop
           #:loop-finish))
