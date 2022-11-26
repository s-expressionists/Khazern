(in-package :cl-user)

(trivial-package-locks:without-package-locks
  (setf (macro-function 'loop) (macro-function 'khazern:loop)
        (macro-function 'loop-finish) (macro-function 'khazern:loop-finish)))
