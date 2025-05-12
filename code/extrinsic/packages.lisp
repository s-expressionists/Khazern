(defpackage #:khazern-extrinsic
  (:use #:common-lisp)
  (:shadow #:loop
           #:loop-finish)
  (:export #:*parser-table*
           #:get-loop-path
           #:loop
           #:loop-finish
           #:remove-loop-path
           #:set-loop-path))
