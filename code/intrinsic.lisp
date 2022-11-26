(in-package :cl-user)

(trivial-package-locks:with-unlocked-packages (:cl)
  (setf (macro-function 'loop) (macro-function 'khazern:loop)
        (macro-function 'loop-finish) (macro-function 'khazern:loop-finish)))
