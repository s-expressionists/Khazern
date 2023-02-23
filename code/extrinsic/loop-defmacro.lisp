(in-package #:khazern-extrinsic)

(defparameter *parser-table* (khazern:copy-parser-table nil))

(defmacro loop-finish ()
  '(go end-loop))

(defmacro loop (&rest forms)
  (khazern:expand-body forms 'end-loop *parser-table*))
