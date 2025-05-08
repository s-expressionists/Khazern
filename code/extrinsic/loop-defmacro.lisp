(in-package #:khazern-extrinsic)

(defparameter *parser-table* (khazern:copy-parser-table nil))

(defmacro loop-finish ()
  '(go end-loop))

(defmacro loop (&rest forms)
  (khazern:expand-body forms 'end-loop *parser-table*))

(defun define-loop-path (pathname-or-names path-function list-of-allowable-prepositions &rest data)
  (khazern::add-path *parser-table* pathname-or-names path-function list-of-allowable-prepositions data))
