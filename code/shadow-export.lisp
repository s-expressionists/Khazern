(cl:in-package #:khazern)

(defparameter *symbols* '(#:loop #:loop-finish))

(cl:loop
   with package = (find-package '#:khazern)
   for symbol in *symbols*
   do (shadow (symbol-name symbol))
      (export (find-symbol (symbol-name symbol) package)))

