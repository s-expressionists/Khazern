(cl:in-package :cl-user)

(trivial-package-locks:without-package-locks
  (setf (macro-function 'cl:loop) (macro-function 'khazern:loop)
        (macro-function 'cl:loop-finish) (macro-function 'khazern:loop-finish)))
