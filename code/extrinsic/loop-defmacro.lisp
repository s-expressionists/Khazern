(in-package #:khazern-extrinsic)

(defmacro loop-finish ()
  '(go end-loop))

(defmacro loop (&rest forms)
  (khazern:expand-body forms 'end-loop))
