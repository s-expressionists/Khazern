(defpackage #:khazern-extension-extrinsic
  (:use #:common-lisp)
  (:nicknames #:kee)
  (:shadow #:loop
           #:loop-finish)
  (:export #:*client*
           #:extrinsic-client
           #:loop
           #:loop-finish))
