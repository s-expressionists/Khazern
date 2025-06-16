(defpackage #:khazern-extrinsic
  (:use #:common-lisp)
  (:nicknames #:ke)
  (:shadow #:loop
           #:loop-finish)
  (:export #:*client*
           #:extrinsic-client
           #:loop
           #:loop-finish))
