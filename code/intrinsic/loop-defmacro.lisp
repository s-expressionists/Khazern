(cl:in-package #:khazern-intrinsic)

(defparameter *parser-table* (khazern:copy-parser-table nil))

(trivial-package-locks:with-unlocked-system-packages

  (defmacro cl:loop-finish ()
    '(go end-loop))

  (defmacro loop (&rest forms)
    (khazern:expand-body forms 'end-loop *parser-table*)))
