(cl:in-package #:common-lisp-user)

(defpackage #:khazern-extension
  (:use #:common-lisp)
  (:export #:define-iteration-path))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :loop/iteration-path/bytes *features*)
  (pushnew :loop/iteration-path/characters *features*)
  (pushnew :loop/iteration-path/elements *features*)
  (pushnew :loop/iteration-path/lines *features*)
  (pushnew :loop/iteration-path/objects *features*))
