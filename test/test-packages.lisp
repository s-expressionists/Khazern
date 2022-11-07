(cl:in-package #:common-lisp-user)

(defpackage #:khazern/test
  (:use #:common-lisp)
  (:shadow #:loop #:loop-finish)
  (:export #:loop-test))
