;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Nov 12 06:30:14 2002
;;;; Contains: Tests of LOOP local variable initialization

(cl:in-package :khazern/test)

(deftest loop.8.1
  (khazern:loop with x = 1 do (return x))
  1)

(deftest loop.8.2
  (khazern:loop with x = 1
        with y = (1+ x) do (return (list x y)))
  (1 2))

(deftest loop.8.3
  (let ((y 2))
    (khazern:loop with x = y
          with y = (1+ x) do (return (list x y))))
  (2 3))

(deftest loop.8.4
  (let (a b)
    (khazern:loop with a = 1
          and b = (list a)
          and c = (list b)
          return (list a b c)))
  (1 (nil) (nil)))

;;; type specs

(deftest loop.8.5
  (khazern:loop with a t = 1 return a)
  1)

(deftest loop.8.6
  (khazern:loop with a fixnum = 2 return a)
  2)

(deftest loop.8.7
  (khazern:loop with a float = 3.0 return a)
  3.0)

(deftest loop.8.8
  (khazern:loop with a of-type string = "abc" return a)
  "abc")

(deftest loop.8.9
  (khazern:loop with (a b) = '(1 2) return (list b a))
  (2 1))

(deftest loop.8.10
  (khazern:loop with (a b) of-type (fixnum fixnum) = '(3 4) return (+ a b))
  7)

(deftest loop.8.11
  (khazern:loop with a of-type fixnum return a)
  0)

(deftest loop.8.12
  (khazern:loop with a of-type float return a)
  0.0)

(deftest loop.8.13
  (khazern:loop with a of-type t return a)
  nil)

(deftest loop.8.14
  (khazern:loop with a t return a)
  nil)

(deftest loop.8.15
  (khazern:loop with a t and b t return (list a b))
  (nil nil))

(deftest loop.8.16
  (khazern:loop with (a b c) of-type (fixnum float t) return (list a b c))
  (0 0.0 nil))

(deftest loop.8.17
  (khazern:loop with nil = nil return nil)
  nil)

;;; The NIL block of a loop encloses the entire loop.

(deftest loop.8.18
  (khazern:loop with nil = (return t) return nil)
  t)

(deftest loop.8.19
  (khazern:loop with (nil a) = '(1 2) return a)
  2)

(deftest loop.8.20
  (khazern:loop with (a nil) = '(1 2) return a)
  1)

(deftest loop.8.21
  (khazern:loop with b = 3
        and (a nil) = '(1 2) return (list a b))
  (1 3))

(deftest loop.8.22
  (khazern:loop with b = 3
        and (nil a) = '(1 2) return (list a b))
  (2 3))

;;; The NIL block of a loop encloses the entire loop.

(deftest loop.8.23
  (khazern:loop
   with a = 1
   and  b = (return 2)
   return 3)
  2)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest loop.8.24
  (macrolet
   ((%m (z) z))
   (khazern:loop with x = (expand-in-current-env (%m 1)) do (return x)))
  1)

;;; Error cases

;;; The spec says (in section 6.1.1.7) that:
;;; "An error of type program-error is signaled (at macro expansion time)
;;;  if the same variable is bound twice in any variable-binding clause
;;;  of a single loop expression. Such variables include local variables,
;;;  iteration control variables, and variables found by destructuring."
;;;
;;; This is somewhat ambiguous.  Test loop.8.error.1 binds A twice in
;;; the same clause, but loop.8.error.2 binds A in two different clauses.
;;; I am interpreting the spec as ruling out the latter as well.

(deftest loop.8.error.1
  (signals-error
   (khazern:loop with a = 1
         and  a = 2 return a)
   program-error)
  t)

(deftest loop.8.error.2
  (signals-error
   (khazern:loop with a = 1
         with a = 2 return a)
   program-error)
  t)
