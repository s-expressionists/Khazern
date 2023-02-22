(cl:in-package #:khazern-intrinsic)

(trivial-package-locks:with-unlocked-system-packages

  (defmacro cl:loop-finish ()
    '(go end-loop))

  (defmacro loop (&rest forms)
    (khazern:expand-body forms 'end-loop)))
